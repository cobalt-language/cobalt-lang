use super::*;

#[test]
fn test_slice_backward() {
    let source = "hello world";
    let mut reader = SourceReader::new(source);
    reader.next_char();
    reader.next_char();
    reader.next_char();
    assert_eq!(reader.slice_backward(3), "hel");
}
