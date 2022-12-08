import std/strformat

type Location* = ref object
  lineNumber, columnNumber: int
  filename: string

proc newLocation*(
  filename: string,
  lineNumber, columnNumber: int,
): Location =
  Location(
    filename: filename,
    lineNumber: lineNumber,
    columnNumber: columnNumber,
  )

proc lineNumber*(self: Location): int =
  self.lineNumber

proc columnNumber*(self: Location): int =
  self.columnNumber

proc filename*(self: Location): string =
  self.filename

proc `$`*(self: Location): string =
  fmt"{self.filename}:{self.lineNumber}:{self.columnNumber}"

if isMainModule:
  echo newLocation("foo", 12, 34)
