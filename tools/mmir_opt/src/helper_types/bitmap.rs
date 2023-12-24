#[derive(Clone)]
pub struct Bitmap(Vec<u32>);
impl Bitmap {
    pub fn new(size: usize) -> Self {
        Bitmap(vec![0; (size + 32-1) / 32])
    }
    pub fn get(&self, idx: usize) -> bool {
        self.0[idx / 32] & (1 << (idx%32)) != 0
    }
    pub fn set(&mut self, idx: usize) {
        self.0[idx / 32] |= 1 << (idx%32);
    }
}