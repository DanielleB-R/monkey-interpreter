use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Scope {
    Builtin,
    Global,
    Local,
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
    pub outer: Option<Box<SymbolTable>>,
    store: HashMap<String, Symbol>,
    pub num_definitions: isize,
}

impl SymbolTable {
    pub fn enclosing(outer: Box<Self>) -> Box<Self> {
        Box::new(Self {
            outer: Some(outer),
            store: Default::default(),
            num_definitions: 0,
        })
    }

    pub fn unenclose(mut self) -> Box<Self> {
        self.outer.take().unwrap()
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = Symbol {
            index: self.num_definitions,
            scope: if self.outer.is_none() {
                Scope::Global
            } else {
                Scope::Local
            },
        };
        self.store.insert(name.to_owned(), symbol);
        self.num_definitions += 1;
        symbol
    }

    pub fn define_builtin(&mut self, index: isize, name: &str) -> Symbol {
        let symbol = Symbol {
            index,
            scope: Scope::Builtin,
        };
        self.store.insert(name.to_owned(), symbol);
        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        self.store.get(name).cloned().or_else(|| match &self.outer {
            Some(o) => o.resolve(name),
            None => None,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_define() {
        let mut global = Box::new(SymbolTable::default());

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

        let mut first_local = SymbolTable::enclosing(global);

        let c = first_local.define("c");
        assert_eq!(
            c,
            Symbol {
                scope: Scope::Local,
                index: 0
            }
        );

        let d = first_local.define("d");
        assert_eq!(
            d,
            Symbol {
                scope: Scope::Local,
                index: 1
            }
        );

        let mut second_local = SymbolTable::enclosing(first_local);

        let e = second_local.define("e");
        assert_eq!(
            e,
            Symbol {
                scope: Scope::Local,
                index: 0
            }
        );

        let f = second_local.define("f");
        assert_eq!(
            f,
            Symbol {
                scope: Scope::Local,
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

    #[test]
    fn test_resolve_nested_local() {
        let mut global = Box::new(SymbolTable::default());
        global.define("a");
        global.define("b");

        let mut first_local = SymbolTable::enclosing(global);
        first_local.define("c");
        first_local.define("d");
        let first_local_saved = first_local.clone();

        let mut second_local = SymbolTable::enclosing(first_local);
        second_local.define("e");
        second_local.define("f");

        let cases = vec![
            (
                first_local_saved,
                vec![
                    (
                        "a",
                        Symbol {
                            scope: Scope::Global,
                            index: 0,
                        },
                    ),
                    (
                        "b",
                        Symbol {
                            scope: Scope::Global,
                            index: 1,
                        },
                    ),
                    (
                        "c",
                        Symbol {
                            scope: Scope::Local,
                            index: 0,
                        },
                    ),
                    (
                        "d",
                        Symbol {
                            scope: Scope::Local,
                            index: 1,
                        },
                    ),
                ],
            ),
            (
                second_local,
                vec![
                    (
                        "a",
                        Symbol {
                            scope: Scope::Global,
                            index: 0,
                        },
                    ),
                    (
                        "b",
                        Symbol {
                            scope: Scope::Global,
                            index: 1,
                        },
                    ),
                    (
                        "e",
                        Symbol {
                            scope: Scope::Local,
                            index: 0,
                        },
                    ),
                    (
                        "f",
                        Symbol {
                            scope: Scope::Local,
                            index: 1,
                        },
                    ),
                ],
            ),
        ];

        for (table, expected) in cases {
            for (name, symbol) in expected {
                assert_eq!(table.resolve(name), Some(symbol));
            }
        }
    }

    #[test]
    fn test_define_resolve_builtins() {
        let mut global = Box::new(SymbolTable::default());

        global.define_builtin(0, "a");
        global.define_builtin(1, "c");
        global.define_builtin(2, "e");
        global.define_builtin(3, "f");

        fn test_resolve_builtins(table: &SymbolTable) {
            assert_eq!(
                table.resolve("a").unwrap(),
                Symbol {
                    scope: Scope::Builtin,
                    index: 0
                },
            );
            assert_eq!(
                table.resolve("c").unwrap(),
                Symbol {
                    scope: Scope::Builtin,
                    index: 1
                },
            );
            assert_eq!(
                table.resolve("e").unwrap(),
                Symbol {
                    scope: Scope::Builtin,
                    index: 2
                },
            );
            assert_eq!(
                table.resolve("f").unwrap(),
                Symbol {
                    scope: Scope::Builtin,
                    index: 3
                },
            );
        };

        test_resolve_builtins(&global);
        let first_local = SymbolTable::enclosing(global);
        test_resolve_builtins(&first_local);
        let second_local = SymbolTable::enclosing(first_local);
        test_resolve_builtins(&second_local);
    }
}
