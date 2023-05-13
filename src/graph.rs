use std::collections::{HashMap, VecDeque};
use semver::{Version, VersionReq, Comparator};
use lasso::{ThreadedRodeo, Spur};
use crate::{pkg, build};
type Id = Spur;
lazy_static::lazy_static! {
    static ref STRINGS: ThreadedRodeo = ThreadedRodeo::new();
}
struct PkgNode {
    pub version: Version,
    pub comps: Vec<(Comparator, (Id, Id))>,
    pub dependencies: Vec<((Id, Id), VersionReq)>,
    pub dependents: usize,
    pub init: bool
}
impl Default for PkgNode {
    fn default() -> Self {
        Self {
            version: Version::new(0, 0, 0),
            comps: vec![],
            dependencies: vec![],
            dependents: 0,
            init: false
        }
    }
}
impl PkgNode {
    /// Update the dependency graph
    /// If this node was already visited, check to see if the same version is still applicable and
    /// if so, recreate the graph.
    pub fn update(&mut self, pkg: Id, target: Id, ctx: &mut DependencyGraph, queue: &mut VecDeque<(Id, Id)>) -> anyhow::Result<()> {
        // find a suitable release
        let (r, version) = ctx.registry.get(&pkg)
            .ok_or_else(|| pkg::InstallError::CantFindPkg(STRINGS.resolve(&pkg)))?
            .releases.iter().filter_map(|(v, p)| {
                let v = Version::parse(v).ok()?;
                self.comps.iter().all(|(c, _)| c.matches(&v)).then_some((p, v))
            }).last()
            .ok_or_else(|| pkg::InstallError::NoMatchingVersion(STRINGS.resolve(&pkg), VersionReq {comparators: self.comps.iter().map(|(c, _)| c.clone()).collect()}))?;
        if self.init && version == self.version {return Ok(())} // everything is fine.
        let proj = r.project(STRINGS.resolve(&pkg), &version, ctx.is_frozen)?;
        // get target
        let mut deps = {
            let target = proj.get_target(STRINGS.resolve(&target)).ok_or_else(|| pkg::InstallError::NoMatchingTarget(STRINGS.resolve(&pkg), self.version.clone(), STRINGS.resolve(&target)))?;
            let mut deps = Vec::new();
            for (k, x) in target.deps.iter() {
                match x {
                    build::Dependency::Project       => deps.push(((pkg, STRINGS.get_or_intern(k)), VersionReq {comparators: vec![Comparator {op: semver::Op::Exact, major: version.major, minor: Some(version.minor), patch: Some(version.patch), pre: version.pre.clone()}]})),
                    build::Dependency::System        => {},
                    build::Dependency::Package(info) => deps.extend(info.targets.as_ref().cloned()
                        .or_else(|| proj.has_target("default").then(|| vec!["default".to_string()]))
                        .ok_or_else(|| pkg::InstallError::NoDefaultTarget(STRINGS.resolve(&pkg)))?.iter()
                        .map(|x| ((STRINGS.get_or_intern(k), STRINGS.get_or_intern(x)), info.version.clone())))
                }
            }
            deps
        };
        // filter dependencies into "good" (still needed) and "bad" (no longer used)
        let (good, bad): (Vec<_>, Vec<_>) = std::mem::take(&mut self.dependencies).into_iter().partition(|(p, _)| deps.iter().any(|(x, _)| x == p));
        deps.retain(|(p, _)| !good.iter().any(|(x, _)| x == p)); // remove any "good" dependencies from the "good" list
        assert!(!self.init || bad.is_empty()); // there should not be any bad packages if this is the first time
        bad.into_iter().for_each(|(p, _)| unsafe {(*(&mut ctx.packages as *mut HashMap<(Id, Id), PkgNode>)).get_mut(&p)}.unwrap().remove_dependent(p, (pkg, target), ctx, queue));
        queue.reserve(good.len() + deps.len());
        good.into_iter().for_each(|(x, v)| {
            let comps = &mut ctx.packages.get_mut(&x).unwrap().comps;
            comps.retain(|(_, i)| i != &x); // delete all comparators from this package...
            comps.extend(v.comparators.into_iter().map(|v| (v, x))); // ...and add the new ones
            queue.push_front(x); // updating these needs to be the priority
        });
        deps.into_iter().for_each(|(p, v)| ctx.packages.entry(p).or_default().add_dependent(p, (pkg, target), v, queue));
        self.init = true;
        Ok(())
    }
    /// Remove one of the dependents for this node
    /// If there are no more dependents, remove this node and all queued dependencies
    /// This uses reference counting, so any cycles won't be removed
    fn remove_dependent(&mut self, this: (Id, Id), deps: (Id, Id), ctx: &mut DependencyGraph, queue: &mut VecDeque<(Id, Id)>) {
        self.dependents -= 1;
        if self.dependents == 0 {
            queue.retain(|x| x != &this);
            ctx.packages.remove(&this);
            self.dependencies.iter().for_each(|(p, _)| unsafe {(*(&mut ctx.packages as *mut HashMap<(Id, Id), PkgNode>)).get_mut(p)}.unwrap().remove_dependent(*p, this, ctx, queue)); // This probably won't cause any issues
        }
        else {self.comps.retain(|(_, x)| x != &deps)};
    }
    /// Add a dependent to this node
    /// If the node didn't already exist (no dependents), it queues itself
    fn add_dependent(&mut self, this: (Id, Id), dep: (Id, Id), ver: VersionReq, queue: &mut VecDeque<(Id, Id)>) {
        if self.dependents == 0 {queue.push_back(this)}
        self.dependents += 1;
        self.comps.extend(ver.comparators.into_iter().map(|v| (v, dep)));
    }
}
pub struct DependencyGraph {
    pub(self) registry: HashMap<Id, &'static pkg::Package>,
    pub(self) packages: HashMap<(Id, Id), PkgNode>,
    pub is_frozen: bool
}
impl DependencyGraph {
    /// Create a new dependency graph
    /// The registry is used from `pkg::REGISTRY`, and `packages` is initially empty
    pub fn new() -> Self {
        Self {
            registry: pkg::REGISTRY.as_ref().expect("registry must be successfully built").iter().map(|pkg| (STRINGS.get_or_intern_static(&pkg.name), pkg)).collect(),
            packages: HashMap::new(),
            is_frozen: false
        }
    }
    /// Create a new dependency graph, but with `is_frozen` set to `true`.
    pub fn frozen() -> Self {
        Self {
            registry: pkg::REGISTRY.as_ref().expect("registry must be successfully built").iter().map(|pkg| (STRINGS.get_or_intern_static(&pkg.name), pkg)).collect(),
            packages: HashMap::new(),
            is_frozen: false
        }
    }
    /// Build the dependency tree from the installation specification
    pub fn build_tree<I: IntoIterator<Item = pkg::InstallSpec>>(&mut self, pkgs: I) -> anyhow::Result<()> {
        let mut node = PkgNode::default();
        node.dependents = 1;
        node.version = Version::new(1, 0, 0);
        let idef = STRINGS.get_or_intern_static("default");
        for pkg::InstallSpec {name, targets, version} in pkgs {
            let iname = STRINGS.get_or_intern(&name);
            if let Some(targets) = targets {
                node.dependencies.reserve(targets.len());
                for target in targets {
                    let itarg = STRINGS.get_or_intern(target);
                    node.dependencies.push(((iname, itarg), version.clone()));
                }
            }
            else {
                let (r, v) = self.registry.get(&iname)
                    .ok_or_else(|| pkg::InstallError::CantFindPkg(STRINGS.resolve(&iname)))?
                    .releases.iter().filter_map(|(v, p)| {
                        let v = Version::parse(v).ok()?;
                        version.matches(&v).then_some((p, v))
                    }).last()
                    .ok_or_else(|| pkg::InstallError::NoMatchingVersion(STRINGS.resolve(&iname), version.clone()))?;
                let proj = r.project(&name, &v, self.is_frozen)?;
                if proj.has_target("default") {
                    node.dependencies.push(((iname, idef), version.clone()));
                }
                else {anyhow::bail!(pkg::InstallError::NoDefaultTarget(STRINGS.resolve(&idef)))}
            }
        }
        let mut queue: VecDeque<(Id, Id)> = [(STRINGS.get_or_intern_static("<installation entry>"), STRINGS.get_or_intern_static("<installation entry>"))].into();
        while let Some((p, t)) = queue.pop_front() {unsafe {(*(&mut self.packages as *mut HashMap<(Id, Id), PkgNode>)).get_mut(&(p, t))}.unwrap().update(p, t, self, &mut queue)?}
        Ok(())
    }
}
