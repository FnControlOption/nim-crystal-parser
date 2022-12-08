from ast import isFloat, NumberKind, StringInterpolation
import error, location, options, token
import std/macros, std/strformat, std/strutils, std/tables

type Lexer* = ref object of RootObj
  docEnabled, commentsEnabled, countWhitespace, wantsRaw, slashIsRegex, wantsDefOrMacroName: bool
  string: string
  currentPos: int
  token, tempToken: Token
  lineNumber, columnNumber: int
  wantsSymbol, wantsRegex, commentIsDoc: bool
  filename, stackedFilename: string
  privateTokenEndLocation: Option[Location]
  # stringPool
  heredocs: seq[(DelimiterState, StringInterpolation)]
  # macroExpansionPragmas
  # warnings

  delimiterStateStack: seq[DelimiterState]
  macroCurlyCount: int

  stacked: bool
  stackedLineNumber, stackedColumnNumber: int

proc newLexer*(
  string: string,
  # stringPool
  # warnings
): Lexer =
  Lexer(
    # warnings
    string: string,
    currentPos: 0,
    token: newToken(),
    tempToken: newToken(),
    lineNumber: 1,
    columnNumber: 1,
    filename: "",
    wantsRegex: true,
    docEnabled: false,
    commentIsDoc: true,
    commentsEnabled: false,
    countWhitespace: false,
    slashIsRegex: true,
    wantsRaw: false,
    wantsDefOrMacroName: false,
    wantsSymbol: true,
    # stringPool

    delimiterStateStack: @[],
    macroCurlyCount: 0,

    stacked: false,
    stackedFilename: "",
    stackedLineNumber: 1,
    stackedColumnNumber: 1,
  )

proc `filename=`*(self: Lexer, filename: string) =
  self.filename = filename

proc `raise`(
  self: Lexer,
  message: string,
  lineNumber = self.lineNumber,
  columNumber = self.columnNumber,
  filename = self.filename,
) {.noReturn.} =
  let e = newException(SyntaxError, message)
  e.lineNumber = lineNumber
  e.columnNumber = columNumber
  e.filename = filename
  raise e

proc `raise`(
  self: Lexer,
  message: string,
  token: Token,
  size = int.none,
) {.noReturn.} =
  let e = newException(SyntaxError, message)
  e.lineNumber = token.lineNumber
  e.columnNumber = token.columnNumber
  e.filename = token.filename
  e.size = size
  raise e

proc `raise`(
  self: Lexer,
  message: string,
  token: Token,
  size: int,
) {.noReturn.} =
  self.`raise` message, token, size.some

proc `raise`(
  self: Lexer,
  message: string,
  location: Location,
) {.noReturn.} =
  self.`raise` message, location.lineNumber, location.columnNumber, location.filename

proc resetToken(self: Lexer) =
  self.token.value = TokenValue(kind: tvNone)
  self.token.lineNumber = self.lineNumber
  self.token.columnNumber = self.columnNumber
  self.token.filename = self.filename
  self.token.location = Location.none
  self.token.passedBackslashNewline = false
  if self.token.kind != tSpace and self.token.kind != tNewline:
    self.token.docBuffer = string.none
  self.privateTokenEndLocation = Location.none

proc incrColumnNumber(self: Lexer, d = 1) =
  self.columnNumber += d
  if self.stacked:
    self.stackedColumnNumber += d

proc incrLineNumber(self: Lexer, columnNumber = 1.some) =
  self.lineNumber += 1
  if columnNumber.isSome:
    self.columnNumber = columnNumber.get
  if self.stacked:
    self.stackedLineNumber += 1
    if columnNumber.isSome:
      self.stackedColumnNumber = columnNumber.get

proc currentChar(self: Lexer): char =
  if self.currentPos < self.string.len:
    result = self.string[self.currentPos]
  else:
    result = '\0'

proc peekNextChar(self: Lexer): char =
  let nextPos = self.currentPos + 1
  if nextPos < self.string.len:
    result = self.string[nextPos]
  else:
    result = '\0'

proc nextCharNoColumnIncrement(self: Lexer): char =
  if self.currentPos < self.string.len:
    self.currentPos += 1
  result = self.currentChar

proc nextChar(self: Lexer): char =
  self.incrColumnNumber
  result = self.nextCharNoColumnIncrement

proc nextCharCheckLine(self: Lexer): char =
  result = self.nextCharNoColumnIncrement
  if result == '\n':
    self.incrLineNumber
  else:
    self.incrColumnNumber

proc nextChar(self: Lexer, tokenKind: TokenKind) =
  discard self.nextChar
  self.token.kind = tokenKind

proc stringRange(self: Lexer, startPos, endPos: int): string =
  result = self.string[startPos ..< endPos]

proc stringRange(self: Lexer, startPos: int): string =
  result = self.stringRange(startPos, self.currentPos)

proc isIdentStart(c: char): bool =
  result = c.isAlphaAscii or c == '_' # or c > 0x9F

proc isIdentPart(c: char): bool =
  result = c.isIdentStart or c.isDigit

proc isIdent(name: string): bool =
  result = name.len > 0 and name[0].isIdentStart

proc isSetter(name: string): bool =
  result = name.isIdent and name.endsWith('=')

proc isIdentPartOrEnd(c: char): bool =
  result = c.isIdentPart or c in {'?', '!'}

proc peekNotIdentPartOrEndNextChar(self: Lexer): bool =
  if not self.peekNextChar.isIdentPartOrEnd and self.peekNextChar != ':':
    discard self.nextChar
    result = true
  else:
    result = false

proc unknownToken(self: Lexer) {.noReturn.} =
  let escaped = self.currentChar.`$`.escape("'", "'")
  self.`raise` fmt"unknown token: {escaped}"

proc tokenEndLocation(self: Lexer): Location =
  if self.privateTokenEndLocation.isNone:
    self.privateTokenEndLocation = newLocation(
      self.filename,
      self.lineNumber,
      self.columnNumber - 1,
    ).some
  result = self.privateTokenEndLocation.get

proc skipComment(self: Lexer) =
  var c = self.currentChar
  while c notin {'\n', '\0'}:
    c = self.nextCharNoColumnIncrement

proc consumeComment(self: Lexer, startPos: int) =
  self.skipComment
  self.token.kind = tComment
  self.token.value = TokenValue(
    kind: tvString,
    string: self.stringRange(startPos),
  )

proc consumeDoc(self: Lexer) =
  var
    c = self.currentChar
    startPos = self.currentPos

  if c == ' ':
    c = self.nextChar
    startPos = self.currentPos

  self.skipComment

  var docBuffer {.noInit.}: string
  if self.token.docBuffer.isSome:
    docBuffer = self.token.docBuffer.get
    docBuffer.add '\n'
  else:
    docBuffer = ""
    self.token.docBuffer = docBuffer.some

  docBuffer.add self.stringRange(startPos)

proc handleCrlfOrLf(self: Lexer): bool =
  result = self.currentChar == '\r'
  if result:
    if self.nextChar != '\n':
      self.`raise` "expecting '\\n' after '\\r'"

proc consumeWhitespace(self: Lexer) =
  let startPos = self.currentPos
  self.token.kind = tSpace
  discard self.nextChar
  while true:
    case self.currentChar
    of ' ', '\t':
      discard self.nextChar
    of '\\':
      if self.nextChar in {'\r', '\n'}:
        discard self.handleCrlfOrLf
        discard self.nextChar
        self.incrLineNumber
        self.token.passedBackslashNewline = true
      else:
        self.unknownToken
    else:
      break
  if self.countWhitespace:
    self.token.value = TokenValue(
      kind: tvString,
      string: self.stringRange(startPos),
    )

proc consumeNewlines(self: Lexer) =
  if self.heredocs.len > 0:
    return

  if self.countWhitespace:
    return

  while true:
    case self.currentChar
    of '\n':
      discard self.nextCharNoColumnIncrement
      self.incrLineNumber(int.none)
      self.token.docBuffer = string.none
    of '\r':
      if self.nextCharNoColumnIncrement != '\n':
        self.`raise` "expected '\\n' after '\\r'"
      discard self.nextCharNoColumnIncrement
      self.incrLineNumber(int.none)
      self.token.docBuffer = string.none
    else:
      break

proc scanIdent(self: Lexer, start: int) =
  while self.currentChar.isIdentPart:
    discard self.nextChar
  if self.currentChar in {'?', '!'} and self.peekNextChar != '=':
    discard self.nextChar
  self.token.kind = tIdent
  self.token.value = TokenValue(
    kind: tvString,
    string: self.stringRange(start),
  )

proc checkIdentOrKeyword(
  self: Lexer,
  keyword: Keyword,
  start: int,
) =
  if self.peekNextChar.isIdentPartOrEnd:
    self.scanIdent(start)
  else:
    discard self.nextChar
    self.token.kind = tIdent
    self.token.value = TokenValue(
      kind: tvKeyword,
      keyword: keyword,
    )

proc symbol(self: Lexer, value: string) =
  self.token.kind = tSymbol
  self.token.value = TokenValue(kind: tvString, string: value)
  if self.wantsRaw:
    self.token.raw = fmt":{value}"

proc setTokenRawFromStart(self: Lexer, start: int) =
  if self.wantsRaw:
    self.token.raw = self.stringRange(start)

proc scanNumber(
  self: Lexer,
  start: int,
  negative = false,
) =
  # TODO: implement
  while self.currentChar.isDigit:
    discard self.nextChar
  self.token.kind = tNumber
  self.setTokenRawFromStart(start)
  self.token.value = TokenValue(
    kind: tvString,
    string: self.stringRange(start),
  )

proc newTokenCase(
  self: NimNode,
  index: int,
  tokenKind: NimNode,
  leaves: seq[NimNode],
): NimNode =
  if leaves.len == 0:
    assert not tokenKind.isNil;
    return quote do: `self`.nextChar `tokenKind`

  type Branch = tuple[tokenKind: NimNode, leaves: seq[NimNode]]
  var branches = initTable[char, Branch]()

  for leaf in leaves:
    let
      raw = leaf[0].strVal
      ch = raw[index]

    if not branches.hasKey ch:
      branches[ch] = (nil, @[])

    if raw.len == index + 1:
      assert branches[ch].tokenKind.isNil
      branches[ch] = (leaf[1], branches[ch][1])
    else:
      branches[ch].leaves.add leaf

  result = newNimNode(nnkCaseStmt)

  result.add:
    quote do: `self`.nextChar

  for ch, branch in branches.pairs:
    result.add:
      let branchStmt = newNimNode(nnkOfBranch)

      branchStmt.add:
        newLit(ch)

      branchStmt.add:
        newTokenCase(
          self,
          index + 1,
          branch.tokenKind,
          branch.leaves,
        )

  result.add:
    newNimNode(nnkElse).add:
      if tokenKind.isNil:
        quote do: `self`.unknownToken
      else:
        quote do: `self`.token.kind = `tokenKind`

macro genTokenCase(
  self: Lexer,
  index: int,
  root: (string, TokenKind),
  leaves: varargs[(string, TokenKind)],
) =
  for leaf in leaves:
    assert leaf[0].strVal.startsWith(root[0].strVal)
  let
    index = cast[int](index.intVal)
    tokenKind = root[1]
    leaves = leaves[0 .. ^1]
  newTokenCase(self, index, tokenKind, leaves)

proc advanceToken(self: Lexer) =
  if self.token.kind == tNewline or self.token.kind == tEof:
    self.commentIsDoc = true
  elif self.token.kind != tSpace:
    self.commentIsDoc = false

  self.resetToken

  # TODO: Skip comments

  var start = self.currentPos

  # macroExpansionPragmas

  var resetRegexFlags = true

  case self.currentChar
  of '\0':
    self.token.kind = tEof
  of ' ', '\t':
    self.consumeWhitespace
    resetRegexFlags = false
  of '\\':
    case self.nextChar
    of '\r', '\n':
      discard self.handleCrlfOrLf
      self.incrLineNumber
      self.token.passedBackslashNewline = true
      self.consumeWhitespace
      resetRegexFlags = false
    else:
      self.unknownToken
  of '\n':
    self.token.kind = tNewline
    discard self.nextChar
    self.incrLineNumber
    resetRegexFlags = false
    self.consumeNewlines
  of '\r':
    if self.nextChar == '\n':
      discard self.nextChar
      self.token.kind = tNewline
      self.incrLineNumber
      resetRegexFlags = false
      self.consumeNewlines
    else:
      self.`raise` "expected '\\n' after '\\r'"
  of '=':
    self.genTokenCase(
      index = 1,
      ("=", tOpEq),
      ("=>", tOpEqGt),
      ("=~", tOpEqTilde),
      ("==", tOpEqEq),
      ("===", tOpEqEqEq),
    )
  of '!':
    self.genTokenCase(
      index = 1,
      ("!", tOpBang),
      ("!=", tOpBangEq),
      ("!~", tOpBangTilde),
    )
  of '<':
    case self.nextChar
    of '=':
      self.genTokenCase(
        index = 2,
        ("<=", tOpLtEq),
        ("<=>", tOpLtEqGt),
      )
    of '<':
      case self.nextChar
      of '=':
        self.nextChar tOpLtLtEq
      of '-':
        # TODO: implement consumeHeredocStart
        self.unknownToken
      else:
        self.token.kind = tOpLtLt
    else:
      self.token.kind = tOpLt
  of '>':
    self.genTokenCase(
      index = 1,
      (">", tOpGt),
      (">=", tOpGtEq),
      (">>", tOpGtGt),
      (">>=", tOpGtGtEq),
    )
  of '+':
    self.token.start = start
    case self.nextChar
    of '=':
      self.nextChar tOpPlusEq
    of '0'..'9':
      self.scanNumber start
    of '+':
      self.`raise` "postfix increment is not supported, use `exp += 1`"
    else:
      self.token.kind = tOpPlus
  of '-':
    self.token.start = start
    case self.nextChar
    of '=':
      self.nextChar tOpMinusEq
    of '>':
      self.nextChar tOpMinusGt
    of '0'..'9':
      self.scanNumber start, negative = true
    of '-':
      self.`raise` "postfix decrement is not supported, use `exp -= 1`"
    else:
      self.token.kind = tOpMinus
  of '*':
    self.genTokenCase(
      index = 1,
      ("*", tOpStar),
      ("*=", tOpStarEq),
      ("**", tOpStarStar),
      ("**=", tOpStarStarEq),
    )
  # TODO
  of '(': self.nextChar tOpLparen
  of ')': self.nextChar tOpRParen
  of '{':
    self.genTokenCase(
      index = 1,
      ("{", tOpLcurly),
      ("{%", tOpLcurlyPercent),
      ("{{", tOpLcurlyLcurly),
    )
  of '}': self.nextChar tOpRcurly
  of '[':
    self.genTokenCase(
      index = 1,
      ("[", tOpLsquare),
      ("[]", tOpLsquareRsquare),
      ("[]=", tOpLsquareRsquareEq),
      ("[]?", tOpLsquareRsquareQuestion),
    )
  of ']': self.nextChar tOpRsquare
  of ',': self.nextChar tOpComma
  of '?': self.nextChar tOpQuestion
  of ';':
    resetRegexFlags = false
    self.nextChar tOpSemicolon
  # TODO
  of '~':
    self.nextChar tOpTilde
  # TODO
  of '0'..'9':
    self.scanNumber start
  # TODO
  else:
    if self.currentChar.isUpperAscii:
      start = self.currentPos
      while self.nextChar.isIdentPart:
        discard
      self.token.kind = tConst
      self.token.value = TokenValue(
        kind: tvString,
        string: self.stringRange(start),
      )
    elif self.currentChar.isIdentStart:
      discard self.nextChar
      self.scanIdent(start)
    else:
      self.unknownToken

  if resetRegexFlags:
    self.wantsRegex = true
    self.slashIsRegex = false

if isMainModule:
  var lexer = newLexer("Foo")
  lexer.advanceToken
  assert $lexer.token == "Foo"

  lexer = newLexer("foo")
  lexer.advanceToken
  assert $lexer.token == "foo"

  lexer = newLexer("=")
  lexer.advanceToken
  assert lexer.token.kind == tOpEq

  lexer = newLexer("=>")
  lexer.advanceToken
  assert lexer.token.kind == tOpEqGt

  lexer = newLexer("==")
  lexer.advanceToken
  assert lexer.token.kind == tOpEqEq

  lexer = newLexer("===")
  lexer.advanceToken
  assert lexer.token.kind == tOpEqEqEq
