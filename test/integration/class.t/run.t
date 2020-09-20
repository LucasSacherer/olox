  $ olox empty.lox
  Foo

  $ olox inherit_self.lox
  [Line 1] Error: No variable called 'Foo'

  $ olox inherited_method.lox
  in Foo
  in Bar
  in Baz

  $ olox local_inherit_other.lox
  B

  $ olox local_inherit_self.lox
  [Line 2] Error: No variable called 'Foo'

  $ olox local_reference_self.lox
  Foo

  $ olox reference_self.lox
  Foo
