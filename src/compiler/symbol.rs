use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Scope {
    Global,
}

impl fmt::Display for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Symbol {
    pub scope: Scope,
    pub index: isize,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SymbolTable {
    outer: Option<Box<SymbolTable>>,
    store: HashMap<String, Symbol>,
    num_definitions: isize,
}

impl SymbolTable {
    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = Symbol {
            index: self.num_definitions,
            scope: Scope::Global,
        };
        self.store.insert(name.to_owned(), symbol);
        self.num_definitions += 1;
        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        self.store.get(name).cloned()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_define() {
        let mut global = SymbolTable::default();

        let a = global.define("a");
        assert_eq!(
            a,
            Symbol {
                scope: Scope::Global,
                index: 0
            }
        );

        let b = global.define("b");
        assert_eq!(
            b,
            Symbol {
                scope: Scope::Global,
                index: 1
            }
        );
    }

    #[test]
    fn test_resolve_global() {
        let mut global = SymbolTable::default();
        global.define("a");
        global.define("b");

        let a = global.resolve("a").expect("name a not resolvable");
        assert_eq!(
            a,
            Symbol {
                scope: Scope::Global,
                index: 0
            }
        );

        let b = global.resolve("b").expect("name b not resolvable");
        assert_eq!(
            b,
            Symbol {
                scope: Scope::Global,
                index: 1
            }
        );
    }
}
