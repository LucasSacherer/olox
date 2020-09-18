  $ olox associativity.lox
  c
  c
  c

  $ olox global.lox
  before
  after
  arg
  arg

  $ olox grouping.lox
  [Line 2] Error at '=': Invalid assignment target!

  $ olox infix_operator.lox
  [Line 3] Error at '=': Invalid assignment target!

  $ olox local.lox
  before
  after
  arg
  arg

  $ olox prefix_operator.lox
  [Line 2] Error at '=': Invalid assignment target!

  $ olox syntax.lox
  var
  var

  $ olox to_this.lox
  [Line 3] Error at '=': Invalid assignment target!
  [Line 4] Error at '}': Expected literal, ident, or group!

  $ olox undefined.lox
  [Line 1] Error: Can't assign to undeclared variable 'unknown'
