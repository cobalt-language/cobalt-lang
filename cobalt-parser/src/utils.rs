/// An iterator that iterates over the bytes in a character.
/// String::from(c) would also work, but that allocates.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CharBytesIterator(u32, u8);
impl CharBytesIterator {
    #[inline]
    pub fn from_char(c: char) -> Self {Self(c as u32, 0)}
    #[inline]
    pub fn from_u8(c: u8) -> Self {Self(c as u32, 0)}
    #[inline]
    pub fn from_u32(c: u32) -> Option<Self> {(c < 0x200000).then_some(Self(c, 0))}
    #[inline]
    pub fn raw(c: u8) -> Self {Self(c as u32, 254)}
    #[inline]
    pub fn explode(self) -> (u32, u8) {(self.0, self.1)}
}
impl Iterator for CharBytesIterator {
    type Item = u8;
    fn next(&mut self) -> Option<u8> {
        let res = match self.explode() {
            (c, 254) => Some(c as u8),
            (c @ 0..=0x7f, 0) => Some(c as u8),
            (c @ 0x80..=0x7ff, 0)       => Some(0xc0 | ((c >>  6) as u8 & 0x1F)),
            (c @ 0x80..=0x7ff, 1)       => Some(0x80 | ((c      ) as u8 & 0x3F)),
            (c @ 0x800..=0xffff, 0)     => Some(0xe0 | ((c >> 12) as u8 & 0x0F)),
            (c @ 0x800..=0xffff, 1)     => Some(0x80 | ((c >>  6) as u8 & 0x3F)),
            (c @ 0x800..=0xffff, 2)     => Some(0x80 | ((c      ) as u8 & 0x3F)),
            (c @ 0x10000..=0x1fffff, 0) => Some(0xf0 | ((c >> 18) as u8 & 0x07)),
            (c @ 0x10000..=0x1fffff, 1) => Some(0x80 | ((c >> 12) as u8 & 0x3F)),
            (c @ 0x10000..=0x1fffff, 2) => Some(0x80 | ((c >>  6) as u8 & 0x3F)),
            (c @ 0x10000..=0x1fffff, 3) => Some(0x80 | ((c      ) as u8 & 0x3F)),
            _ => None
        };
        self.1 += res.is_some() as u8;
        res
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let mut val = if self.1 == 254 {1} else {
            match self.0 {
                0..=0x7f => 1,
                0x80..=0x7ff => 2,
                0x800..=0xffff => 3,
                0x10000..=0x1fffff => 4,
                _ => 0
            }
        };
        if val > self.1 {val -= self.1}
        (val as _, Some(val as _))
    }
}
impl std::iter::FusedIterator for CharBytesIterator {}
impl std::iter::ExactSizeIterator for CharBytesIterator {}
