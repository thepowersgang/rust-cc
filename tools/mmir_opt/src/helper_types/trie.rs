
pub struct Trie<T>(Option<TrieNode<T>>);
impl<T> Trie<T>
where
    T: Ord + Eq + Copy
{
    pub fn new() -> Self {
        Trie(None)
    }
    pub fn push(&mut self, p: &[T]) {
        match self.0
        {
        None => self.0 = Some(TrieNode { prefix: p.to_owned(), children: Vec::new(), leaf: true }),
        Some(ref mut v) => v.push(p),
        }
    }
    pub fn iter(&self) -> impl Iterator<Item=Vec<&T>> {
        [].into_iter()
    }
    pub fn dump(&self)
    where
        T: ::std::fmt::Debug,
    {
        if let Some(v) = &self.0 {
            v.dump(0);
        }
        else {
            println!("[]");
        }
    }
}

struct TrieNode<T> {
    prefix: Vec<T>,
    children: Vec<(T, TrieNode<T>)>,
    leaf: bool,
}
impl<T> TrieNode<T>
where
    T: Eq + Ord + Copy,
{
    fn push(&mut self, vals: &[T])
    {
        if self.prefix.len() <= vals.len() && self.prefix == &vals[..self.prefix.len()] {
            if vals.len() == self.prefix.len() {
                self.leaf = true;
            }
            else {
                let v = vals[self.prefix.len()];
                let tail = &vals[self.prefix.len()+1..];
                match self.children.binary_search_by_key(&v, |v| v.0) {
                Ok(i) => self.children[i].1.push(tail),
                Err(i) => self.children.insert(i, (v, TrieNode { prefix: tail.to_owned(), children: Vec::new(), leaf: true })),
                }
            }
        }
        else {
            // Determine the leading match
            let match_len = Iterator::zip(vals.iter(), self.prefix.iter()).take_while(|(a,b)| a==b).count();
            if match_len == 0 {
            }
            else {
                let new_exist = TrieNode {
                    prefix: self.prefix[match_len+1..].to_owned(),
                    children: ::std::mem::take(&mut self.children),
                    leaf: self.leaf
                };
                let new_incoming = TrieNode {
                    prefix: vals[match_len+1..].to_owned(),
                    children: Vec::new(),
                    leaf: true
                };
                self.children = vec![
                    (self.prefix[match_len], new_exist),
                    (vals[match_len], new_incoming),
                ];
                self.children.sort_unstable_by_key(|v| v.0);
                self.prefix = vals[..match_len].to_owned();
                self.leaf = false;
            }
        }
    }
    fn dump(&self, indent: usize)
    where
        T: ::std::fmt::Debug,
    {
        for _ in 0..indent { print!(" ") }
        println!("{:?} = {}", self.prefix, self.leaf);
        for (val,child) in &self.children {
            for _ in 0..indent { print!(" ") }
            println!("=> {:?}", val);
            child.dump(indent+1);
        }
    }
}