use crate::*;
pub fn mark_move<'src, 'ctx>(
    val: &Value<'src, 'ctx>,
    inst: cfg::Location<'ctx>,
    ctx: &CompCtx<'src, 'ctx>,
    loc: SourceSpan,
) {
    if !ctx.is_const.get() {
        if let (Some(name), true) = (
            &val.name,
            ctx.flags.all_move_metadata
                || val.data_type.has_dtor()
                || val.data_type.nom_info().map_or(false, |v| v.is_linear_type),
        ) {
            ctx.moves.borrow_mut().0.insert(cfg::Use {
                is_move: true,
                name: name.clone(),
                real: !ctx.flags.all_move_metadata
                    || val.data_type.has_dtor()
                    || val.data_type.nom_info().map_or(false, |v| v.is_linear_type),
                inst,
                loc,
            });
        }
    }
}
pub fn mark_use<'src, 'ctx>(
    val: &Value<'src, 'ctx>,
    inst: cfg::Location<'ctx>,
    ctx: &CompCtx<'src, 'ctx>,
    loc: SourceSpan,
) {
    if !ctx.is_const.get() {
        if let (Some(name), true) = (
            &val.name,
            ctx.flags.all_move_metadata
                || val.data_type.has_dtor()
                || val.data_type.nom_info().map_or(false, |v| v.is_linear_type),
        ) {
            ctx.moves.borrow_mut().0.insert(cfg::Use {
                is_move: false,
                name: name.clone(),
                real: !ctx.flags.all_move_metadata
                    || val.data_type.has_dtor()
                    || val.data_type.nom_info().map_or(false, |v| v.is_linear_type),
                inst,
                loc,
            });
        }
    }
}
