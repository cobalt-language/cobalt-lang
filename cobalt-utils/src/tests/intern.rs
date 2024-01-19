#[test]
fn test_int_bool_tuple() {
    let intern = crate::Interner::<(i32, bool)>::new();
    {
        let p1 = intern.intern((16, true));
        let p2 = intern.intern((64, false));
        let p3 = intern.intern((16, true));
        assert!(!std::ptr::eq(p1, p2));
        assert!(!std::ptr::eq(p2, p3));
        assert!(std::ptr::eq(p1, p3));
    }
}
