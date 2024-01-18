#[test]
fn test_int_bool_tuple() {
    static INTERN: crate::Interner<(i32, bool)> = crate::Interner::new();
    {
        let p1 = INTERN.intern((16, true));
        let p2 = INTERN.intern((64, false));
        let p3 = INTERN.intern((16, true));
        assert!(!std::ptr::eq(p1, p2));
        assert!(!std::ptr::eq(p2, p3));
        assert!(std::ptr::eq(p1, p3));
    }
}
