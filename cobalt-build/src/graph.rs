#![allow(dead_code)]
use crate::{build, pkg};
use lasso::{Spur, ThreadedRodeo};
use semver::{Comparator, Version, VersionReq};
use std::collections::{HashMap, VecDeque};
pub type Id = Spur;
lazy_static::lazy_static! {
    pub static ref STRINGS: ThreadedRodeo = ThreadedRodeo::new();
}
struct PkgNode {
    pub version: Version,
    pub comps: Vec<(Comparator, (Id, Id))>,
    pub dependencies: Vec<((Id, Id), VersionReq)>,
    pub dependents: usize,
    pub init: bool,
}
impl Default for PkgNode {
    fn default() -> Self {
        Self {
            version: Version::new(0, 0, 0),
            comps: vec![],
            dependencies: vec![],
            dependents: 0,
            init: false,
        }
    }
}
impl PkgNode {
    /// Update the dependency graph
    /// If this node was already visited, check to see if the same version is still applicable and
    /// if so, recreate the graph.
    pub fn update(
        &mut self,
        pkg: Id,
        target: Id,
        ctx: &mut DependencyGraph,
        queue: &mut VecDeque<(Id, Id)>,
    ) -> anyhow::Result<()> {
        // find a suitable release
        let (r, version) = ctx
            .registry
            .get(&pkg)
            .ok_or_else(|| pkg::InstallError::CantFindPkg(STRINGS.resolve(&pkg)))?
            .releases
            .iter()
            .filter_map(|(v, p)| {
                self.comps
                    .iter()
                    .all(|(c, _)| c.matches(v))
                    .then_some((p, v))
            })
            .last()
            .ok_or_else(|| {
                pkg::InstallError::NoMatchingVersion(
                    STRINGS.resolve(&pkg),
                    VersionReq {
                        comparators: self.comps.iter().map(|(c, _)| c.clone()).collect(),
                    },
                )
            })?;
        if self.init && version == &self.version {
            return Ok(());
        } // everything is fine.
        let proj = r.project(STRINGS.resolve(&pkg), version, ctx.is_frozen)?;
        // get target
        let mut deps = {
            let target = proj.targets.get(STRINGS.resolve(&target)).ok_or_else(|| {
                pkg::InstallError::NoMatchingTarget(
                    STRINGS.resolve(&pkg),
                    self.version.clone(),
                    STRINGS.resolve(&target),
                )
            })?;
            let mut deps = Vec::new();
            for (k, x) in target.deps.iter() {
                match x {
                    build::Dependency::Project => deps.push((
                        (pkg, STRINGS.get_or_intern(k)),
                        VersionReq {
                            comparators: vec![Comparator {
                                op: semver::Op::Exact,
                                major: version.major,
                                minor: Some(version.minor),
                                patch: Some(version.patch),
                                pre: version.pre.clone(),
                            }],
                        },
                    )),
                    build::Dependency::System => {}
                    build::Dependency::Package(info) => deps.extend(
                        info.targets
                            .as_ref()
                            .cloned()
                            .or_else(|| {
                                proj.targets
                                    .contains_key("default")
                                    .then(|| vec!["default".into()])
                            })
                            .ok_or_else(|| {
                                pkg::InstallError::NoDefaultTarget(STRINGS.resolve(&pkg))
                            })?
                            .iter()
                            .map(|x| {
                                (
                                    (STRINGS.get_or_intern(k), STRINGS.get_or_intern(x)),
                                    info.version.clone(),
                                )
                            }),
                    ),
                }
            }
            deps
        };
        // filter dependencies into "good" (still needed) and "bad" (no longer used)
        let (good, bad): (Vec<_>, Vec<_>) = std::mem::take(&mut self.dependencies)
            .into_iter()
            .partition(|(p, _)| deps.iter().any(|(x, _)| x == p));
        deps.retain(|(p, _)| !good.iter().any(|(x, _)| x == p)); // remove any "good" dependencies from the "good" list
        assert!(!self.init || bad.is_empty()); // there should not be any bad packages if this is the first time
        bad.into_iter().for_each(|(p, _)| {
            unsafe { (*(&mut ctx.packages as *mut HashMap<(Id, Id), PkgNode>)).get_mut(&p) }
                .unwrap()
                .remove_dependent(p, (pkg, target), ctx, queue)
        });
        queue.reserve(good.len() + deps.len());
        good.into_iter().for_each(|(x, v)| {
            let comps = &mut ctx.packages.get_mut(&x).unwrap().comps;
            comps.retain(|(_, i)| i != &x); // delete all comparators from this package...
            comps.extend(v.comparators.into_iter().map(|v| (v, x))); // ...and add the new ones
            queue.push_front(x); // updating these needs to be the priority
        });
        deps.into_iter().for_each(|(p, v)| {
            ctx.packages
                .entry(p)
                .or_default()
                .add_dependent(p, (pkg, target), v, queue)
        });
        self.init = true;
        Ok(())
    }
    /// Remove one of the dependents for this node
    /// If there are no more dependents, remove this node and all queued dependencies
    /// This uses reference counting, so any cycles won't be removed
    fn remove_dependent(
        &mut self,
        this: (Id, Id),
        deps: (Id, Id),
        ctx: &mut DependencyGraph,
        queue: &mut VecDeque<(Id, Id)>,
    ) {
        self.dependents -= 1;
        if self.dependents == 0 {
            queue.retain(|x| x != &this);
            ctx.packages.remove(&this);
            self.dependencies.iter().for_each(|(p, _)| {
                unsafe { (*(&mut ctx.packages as *mut HashMap<(Id, Id), PkgNode>)).get_mut(p) }
                    .unwrap()
                    .remove_dependent(*p, this, ctx, queue)
            }); // This probably won't cause any issues
        } else {
            self.comps.retain(|(_, x)| x != &deps)
        };
    }
    /// Add a dependent to this node
    /// If the node didn't already exist (no dependents), it queues itself
    fn add_dependent(
        &mut self,
        this: (Id, Id),
        dep: (Id, Id),
        ver: VersionReq,
        queue: &mut VecDeque<(Id, Id)>,
    ) {
        if self.dependents == 0 {
            queue.push_back(this)
        }
        self.dependents += 1;
        self.comps
            .extend(ver.comparators.into_iter().map(|v| (v, dep)));
    }
    /// Use a DFS to find a cycle in the packages
    /// While a valid graph can be created with cycles, a proper build order cannot.
    pub(self) fn find_cycle(
        &self,
        start: (Id, Id),
        ctx: &DependencyGraph,
    ) -> Option<VecDeque<(Id, Id)>> {
        if self.dependencies.iter().any(|(d, _)| d == &start) {
            return Some(VecDeque::new());
        }
        self.dependencies.iter().find_map(|(d, _)| {
            let mut deque = ctx.packages.get(d)?.find_cycle(start, ctx)?;
            deque.push_front(*d);
            Some(deque)
        })
    }
}
pub struct DependencyGraph {
    pub(self) registry: HashMap<Id, &'static pkg::Package>,
    pub(self) packages: HashMap<(Id, Id), PkgNode>,
    pub is_frozen: bool,
}
impl DependencyGraph {
    /// Create a new dependency graph
    /// The registry is used from `pkg::REGISTRY`, and `packages` is initially empty
    pub fn new() -> Self {
        Self {
            registry: pkg::REGISTRY
                .iter()
                .map(|pkg| (STRINGS.get_or_intern_static(&pkg.name), pkg))
                .collect(),
            packages: HashMap::new(),
            is_frozen: false,
        }
    }
    /// Create a new dependency graph, but with `is_frozen` set to `true`.
    pub fn frozen() -> Self {
        Self {
            registry: pkg::REGISTRY
                .iter()
                .map(|pkg| (STRINGS.get_or_intern_static(&pkg.name), pkg))
                .collect(),
            packages: HashMap::new(),
            is_frozen: false,
        }
    }
    /// Clear the DependencyGraph
    /// This is cheaper than creating a new one because the HashMap registry doesn't need to be
    /// rebuilt.
    pub fn clear(&mut self) {
        self.packages.clear()
    }
    /// Build the dependency tree from the installation specification
    #[allow(unreachable_code, unused_variables)]
    pub fn build_tree<I: IntoIterator<Item = pkg::InstallSpec>>(
        &mut self,
        pkgs: I,
    ) -> anyhow::Result<()> {
        unimplemented!("package management is hard, okay?");
        let mut node = PkgNode {
            dependents: 1,
            version: Version::new(1, 0, 0),
            ..PkgNode::default()
        };
        let idef = STRINGS.get_or_intern_static("default");
        let ientry = STRINGS.get_or_intern_static("<installation entry>");
        for pkg::InstallSpec {
            name,
            targets,
            version,
        } in pkgs
        {
            let iname = STRINGS.get_or_intern(&name);
            if let Some(targets) = targets {
                node.dependencies.reserve(targets.len());
                for target in targets {
                    let itarg = STRINGS.get_or_intern(target);
                    node.dependencies.push(((iname, itarg), version.clone()));
                }
            } else {
                let (r, v) = self
                    .registry
                    .get(&iname)
                    .ok_or_else(|| pkg::InstallError::CantFindPkg(STRINGS.resolve(&iname)))?
                    .releases
                    .iter()
                    .filter_map(|(v, p)| version.matches(v).then_some((p, v)))
                    .last()
                    .ok_or_else(|| {
                        pkg::InstallError::NoMatchingVersion(
                            STRINGS.resolve(&iname),
                            version.clone(),
                        )
                    })?;
                let proj = r.project(&name, v, self.is_frozen)?;
                if proj.targets.contains_key("default") {
                    node.dependencies.push(((iname, idef), version.clone()));
                } else {
                    anyhow::bail!(pkg::InstallError::NoDefaultTarget(STRINGS.resolve(&idef)))
                }
            }
        }
        self.packages.insert((ientry, ientry), node);
        let mut queue: VecDeque<(Id, Id)> = [(ientry, ientry)].into();
        while let Some((p, t)) = queue.pop_front() {
            unsafe { (*(&mut self.packages as *mut HashMap<(Id, Id), PkgNode>)).get_mut(&(p, t)) }
                .unwrap()
                .update(p, t, self, &mut queue)?
        }
        Ok(())
    }
    /// Find a suitable build order from the built dependency tree
    /// If this fails, it returns at least one cycle in the graph
    /// The DependencyGraph will be left in the empty state after a call to this function
    /// This implementation uses Kahn's algorithm
    pub fn build_order(
        &mut self,
    ) -> Result<VecDeque<(Id, Id, Version)>, VecDeque<(Id, Id, Version)>> {
        let ientry = STRINGS.get_or_intern_static("<installation entry>");
        let mut nodes: VecDeque<(Id, Id)> = [(ientry, ientry)].into();
        let mut out: VecDeque<(Id, Id, Version)> = VecDeque::new();
        while let Some(node) = nodes.pop_front() {
            let n = self.packages.remove(&node).unwrap();
            out.push_back((node.0, node.1, n.version));
            for (dep, _) in n.dependencies.iter() {
                let node = &mut self.packages.get_mut(dep).unwrap();
                node.dependents -= 1;
                if node.dependents == 0 {
                    nodes.push_back(*dep)
                }
            }
        }
        if self.packages.is_empty() {
            Ok(out)
        } else {
            let out = self
                .packages
                .iter()
                .find_map(|(&k, x)| {
                    let mut out = x.find_cycle(k, self)?;
                    out.push_front(k);
                    Some(
                        out.into_iter()
                            .map(|(p, t)| (p, t, self.packages[&(p, t)].version.clone()))
                            .collect(),
                    )
                })
                .unwrap();
            self.packages.clear(); // Just to be sure that the result is in a clean state
            Err(out)
        }
    }
}
impl Default for DependencyGraph {
    fn default() -> Self {
        Self::new()
    }
}
