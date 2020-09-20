  $ olox bool.lox
  [Line 1] Error: Can only call functions and classes, got Bool: true

  $ olox nil.lox
  [Line 1] Error: Can only call functions and classes, got Nil

  $ olox num.lox
  [Line 1] Error: Can only call functions and classes, got Float: 123.000000

  $ olox object.lox
  [Line 4] Error: Can only call functions and classes, got Instance<Class<name:Foo super:() methods:{}>>

  $ olox string.lox
  [Line 1] Error: Can only call functions and classes, got String: 'str'
