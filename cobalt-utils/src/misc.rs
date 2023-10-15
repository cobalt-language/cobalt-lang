#![allow(clippy::needless_lifetimes)]
/// "Safer" version of transmute that only changes the lifetime of a reference
/// # Safety
/// The lifetime must be valid.
pub unsafe fn new_lifetime<'a, 'b, T: ?Sized>(val: &'a T) -> &'b T {
    std::mem::transmute(val)
}
/// "Safer" version of transmute that only changes the lifetime of a reference
/// Same as [`new_lifetime`], but for mutable references
/// # Safety
/// The lifetime must be valid.
pub unsafe fn new_lifetime_mut<'a, 'b, T: ?Sized>(val: &'a mut T) -> &'b mut T {
    std::mem::transmute(val)
}

/// Function form of dbg!, good for use in map()
pub fn dbg<T: std::fmt::Debug>(val: T) -> T {
    println!("{val:#?}");
    val
}

/// Same as [`dbg`], but supports adding a message. Supports currying so it can be easily used in map()
pub fn dbg_msg<M: std::fmt::Display, T: std::fmt::Debug>(msg: M) -> impl Fn(T) -> T {
    move |val: T| {
        println!("{msg}: {val:#?}");
        val
    }
}
