import std/options

export options

type SyntaxError* = object of CatchableError
  lineNumber*, columnNumber*: int
  filename*: string
  size*: Option[int]
