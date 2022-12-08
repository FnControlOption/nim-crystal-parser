import std/hashes, std/macros

macro defHashBase*(Self: untyped) =
  quote do:
    method hash(self: `Self`): Hash {.base.} =
      quit("to implement!")

macro defHash*(Self: untyped, fields: varargs[untyped]) =
  let self = ident("self")
  let h = ident("h")

  let stmtList = newStmtList()
  for field in fields:
    stmtList.add quote do:
      `h` = `h` !& `self`.`field`.hash

  quote do:
    method hash*(`self`: `Self`): Hash =
      var `h`: Hash = 0
      `stmtList`
      result = !$`h`

macro defEquals*(Self: untyped, fields: varargs[untyped]) =
  let self = ident("self")
  let other = ident("other")

  let stmtList = newStmtList()
  for field in fields:
    stmtList.add quote do:
      if `self`.`field` != `other`.`field`: return false

  quote("@") do:
    proc `==`*(`@self`, `@other`: `@Self`): bool =
      `@stmtList`
      result = true

template defEqualsAndHash*(Self: untyped, fields: varargs[untyped]) =
  defEquals(Self, fields)
  defHash(Self, fields)

type Hashable = ref object of RootObj
defHashBase Hashable
type Foo = ref object of Hashable
  foo: string
defEqualsAndHash Foo, foo
proc newFoo(foo: string): Foo = Foo(foo: foo)
type Bar = ref object of Hashable
  fizz, buzz: string
defEqualsAndHash Bar, fizz, buzz
proc newBar(fizz, buzz: string): Bar = Bar(fizz: fizz, buzz: buzz)
if isMainModule:
  assert newFoo("fizz").hash != 0
  assert newFoo("fizz").hash != newFoo("buzz").hash
  assert newFoo("foo") == newFoo("foo")
  assert newFoo("foo") != newFoo("bar")
  assert newBar("fizz", "buzz").hash != newBar("buzz", "fizz").hash
  assert newBar("fizz", "buzz") != newBar("buzz", "fizz")
