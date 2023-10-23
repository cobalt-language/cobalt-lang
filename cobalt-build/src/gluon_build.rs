use std::path::PathBuf;

use super::build::*;
use gluon::base::types::ArcType;
use gluon::vm::api::de::*;
use gluon::vm::api::ser::*;
use gluon::vm::api::*;
use gluon::vm::impl_getable_simple;
use gluon::*;
use serde::*;

impl VmType for Project {
    type Type = Self;
    fn make_type(vm: &Thread) -> ArcType {
        use std::collections::BTreeMap;
        field_decl! { name, version, author, co_version, desc, targets }
        type This = record_type!(
            name => String,
            version => String,
            author => Option<String>,
            co_version => Option<String>,
            desc => Option<String>,
            targets => BTreeMap<String, Target>,
        );
        This::make_type(vm)
    }
}
impl<'vm, 'value> Getable<'vm, 'value> for Project {
    impl_getable_simple!();
    fn from_value(vm: &'vm Thread, value: vm::Variants<'value>) -> Self {
        De::<Self>::from_value(vm, value).0
    }
}
impl<'vm> Pushable<'vm> for Project {
    fn vm_push(self, context: &mut ActiveThread<'vm>) -> vm::Result<()> {
        Ser(self).vm_push(context)
    }
}
impl VmType for BuildOptions<'_> {
    type Type = BuildOptions<'static>;
    fn make_type(vm: &Thread) -> ArcType {
        field_decl! {
            source_dir,
            build_dir,
            continue_build,
            continue_comp,
            rebuild,
            no_default_link,
            triple,
            profile,
            link_dirs
        };
        type This = record_type!(
            source_dir => PathBuf,
            build_dir => PathBuf,
            continue_build => bool,
            continue_comp => bool,
            rebuild => bool,
            no_default_link => bool,
            triple => String,
            profile => String,
            link_dirs => Vec<PathBuf>,
        );
        This::make_type(vm)
    }
}
impl<'vm, 'value> Getable<'vm, 'value> for BuildOptions<'static> {
    impl_getable_simple!();
    fn from_value(vm: &'vm Thread, value: vm::Variants<'value>) -> Self {
        De::<Self>::from_value(vm, value).0
    }
}
impl<'vm> Pushable<'vm> for BuildOptions<'_> {
    fn vm_push(self, context: &mut ActiveThread<'vm>) -> vm::Result<()> {
        Ser(self).vm_push(context)
    }
}
impl VmType for PkgDepSpec {
    type Type = Self;
    fn make_type(vm: &Thread) -> ArcType {
        field_decl! { version, targets };
        type This = record_type!(
            version => String,
            targets => Option<Vec<String>>
        );
        This::make_type(vm)
    }
}
impl<'vm, 'value> Getable<'vm, 'value> for PkgDepSpec {
    impl_getable_simple!();
    fn from_value(vm: &'vm Thread, value: vm::Variants<'value>) -> Self {
        De::<Self>::from_value(vm, value).0
    }
}
impl<'vm> Pushable<'vm> for PkgDepSpec {
    fn vm_push(self, context: &mut ActiveThread<'vm>) -> vm::Result<()> {
        Ser(self).vm_push(context)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildConfig<'a> {
    pub project: Project,
    pub options: BuildOptions<'a>,
}
impl VmType for BuildConfig<'_> {
    type Type = BuildConfig<'static>;
    fn make_type(vm: &Thread) -> ArcType {
        field_decl! { project, options };
        type This = record_type!(
            project => Project,
            options => BuildOptions<'static>
        );
        This::make_type(vm)
    }
}
impl<'vm, 'value> Getable<'vm, 'value> for BuildConfig<'_> {
    impl_getable_simple!();
    fn from_value(vm: &'vm Thread, value: vm::Variants<'value>) -> Self {
        De::from_value(vm, value).0
    }
}
impl<'vm> Pushable<'vm> for BuildConfig<'_> {
    fn vm_push(self, context: &mut ActiveThread<'vm>) -> vm::Result<()> {
        Ser(self).vm_push(context)
    }
}
impl<'vm> Pushable<'vm> for &BuildConfig<'_> {
    fn vm_push(self, context: &mut ActiveThread<'vm>) -> vm::Result<()> {
        Ser(self).vm_push(context)
    }
}
