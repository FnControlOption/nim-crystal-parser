from ast import isFloat, NumberKind, StringInterpolation
import error, location, options, token
import std/macros, std/strformat, std/strutils, std/tables

type Lexer* = ref object of RootObj
  docEnabled*, commentsEnabled, countWhitespace, wantsRaw, slashIsRegex*, wantsDefOrMacroName*: bool
  string: string
  currentPos*: int
  token*, tempToken: Token
  lineNumber, columnNumber: int
  wantsSymbol, wantsRegex*, commentIsDoc: bool
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

method nextToken*(self: Lexer) {.base.}

proc initLexer*(
  self: Lexer,
  s: string,
  # stringPool
  # warnings
) =
  # warnings
  self.string = s
  self.currentPos = 0
  self.token = newToken()
  self.tempToken = newToken()
  self.lineNumber = 1
  self.columnNumber = 1
  self.filename = ""
  self.wantsRegex = true
  self.docEnabled = false
  self.commentIsDoc = true
  self.commentsEnabled = false
  self.countWhitespace = false
  self.slashIsRegex = true
  self.wantsRaw = false
  self.wantsDefOrMacroName = false
  self.wantsSymbol = true
  # stringPool

  self.delimiterStateStack = @[]
  self.macroCurlyCount = 0

  self.stacked = false
  self.stackedFilename = ""
  self.stackedLineNumber = 1
  self.stackedColumnNumber = 1

proc newLexer*(s: string): Lexer =
  new(result)
  result.initLexer(s)

proc `filename=`*(self: Lexer, filename: string) =
  self.filename = filename

proc `raise`*(
  self: Lexer,
  message: string,
  lineNumber = self.lineNumber,
  columnNumber = self.columnNumber,
  filename = self.filename,
) {.noReturn.} =
  raise (ref SyntaxError)(
    msg: message,
    lineNumber: lineNumber,
    columnNumber: columnNumber,
    filename: filename,
  )

proc `raise`*(
  self: Lexer,
  message: string,
  token: Token,
  size = int.none,
) {.noReturn.} =
  raise (ref SyntaxError)(
    msg: message,
    lineNumber: token.lineNumber,
    columnNumber: token.columnNumber,
    filename: token.filename,
    size: size,
  )

proc `raise`*(
  self: Lexer,
  message: string,
  token: Token,
  size: int,
) {.noReturn.} =
  self.`raise`message, token, size.some

proc `raise`*(
  self: Lexer,
  message: string,
  location: Location,
) {.noReturn.} =
  self.`raise`message, location.lineNumber, location.columnNumber, location.filename

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

proc currentChar*(self: Lexer): char =
  if self.currentPos < self.string.len:
    result = self.string[self.currentPos]
  else:
    result = '\0'

proc peekNextChar*(self: Lexer): char =
  let nextPos = self.currentPos + 1
  if nextPos < self.string.len:
    result = self.string[nextPos]
  else:
    result = '\0'

proc nextCharNoColumnIncrement*(self: Lexer): char =
  if self.currentPos < self.string.len:
    self.currentPos += 1
  result = self.currentChar

proc nextChar*(self: Lexer): char =
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

proc isSetter*(name: string): bool =
  result = name.isIdent and name.endsWith('=')

proc isIdentPartOrEnd(c: char): bool =
  result = c.isIdentPart or c in {'?', '!'}

proc peekNotIdentPartOrEndNextChar(self: Lexer): bool =
  if not self.peekNextChar.isIdentPartOrEnd and self.peekNextChar != ':':
    discard self.nextChar
    result = true
  else:
    result = false

proc closingChar(c: char): char =
  case c
  of '<': result = '>'
  of '(': result = ')'
  of '[': result = ']'
  of '{': result = '}'
  else: result = c

proc closingChar(self: Lexer): char =
  self.currentChar.closingChar

proc skipSpace*(self: Lexer) =
  while self.token.kind == tSpace:
    self.nextToken

proc skipSpaceOrNewline*(self: Lexer) =
  while self.token.kind == tSpace or
      self.token.kind == tNewline:
    self.nextToken

proc skipStatementEnd*(self: Lexer) =
  while self.token.kind == tSpace or
      self.token.kind == tNewline or
      self.token.kind == tOpSemicolon:
    self.nextToken

proc nextTokenSkipSpace*(self: Lexer) =
  self.nextToken
  self.skipSpace

proc nextTokenSkipSpaceOrNewline*(self: Lexer) =
  self.nextToken
  self.skipSpaceOrNewline

proc nextTokenSkipStatementEnd*(self: Lexer) =
  self.nextToken
  self.skipStatementEnd

proc nextTokenNeverASymbol*(self: Lexer) =
  self.wantsSymbol = false
  self.nextToken
  self.wantsSymbol = true

proc handleCrlfOrLf(self: Lexer): bool =
  result = self.currentChar == '\r'
  if result:
    if self.nextChar != '\n':
      self.`raise`"expecting '\\n' after '\\r'"

proc charSequence(
  self: Lexer,
  tokens: varargs[char],
  columnIncrement: bool,
): bool =
  for token in tokens:
    if token != (
      if columnIncrement: self.nextChar
      else: self.nextCharNoColumnIncrement
    ):
      return false
  result = true

proc charSequence(self: Lexer, tokens: varargs[char]): bool =
  self.charSequence(tokens, columnIncrement = true)

proc unknownToken(self: Lexer) {.noReturn.} =
  let escaped = self.currentChar.`$`.escape("'", "'")
  self.`raise`fmt"unknown token: {escaped}"

proc setTokenRawFromStart(self: Lexer, start: int) =
  if self.wantsRaw:
    self.token.raw = self.stringRange(start)

proc tokenEndLocation*(self: Lexer): Location =
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
        self.`raise`"expected '\\n' after '\\r'"
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

proc symbol(self: Lexer, value: string) =
  self.token.kind = tSymbol
  self.token.value = TokenValue(kind: tvString, string: value)
  if self.wantsRaw:
    self.token.raw = fmt":{value}"

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

proc delimitedPair(
  self: Lexer,
  kind: DelimiterKind,
  stringNest, stringEnd: char,
  start: int,
  allowEscapes, advance = true,
) =
  if advance:
    discard self.nextChar
  self.token.kind = tDelimiterStart
  self.token.delimiterState = newDelimiterState(
    kind,
    stringNest,
    stringEnd,
    allowEscapes,
  )
  self.setTokenRawFromStart(start)

proc consumeVariable(
  self: Lexer,
  tokenKind: TokenKind,
  start: int,
) =
  if self.currentChar.isIdentStart:
    while self.nextChar.isIdentPart:
      discard
    self.token.kind = tokenKind
    self.token.value = TokenValue(
      kind: tvString,
      string: self.stringRange(start),
    )
  else:
    self.unknownToken

proc newTokenCase(
  self, kind, start: NimNode,
  onTokenFound: proc (self, kind, start: NimNode): NimNode,
  onTokenMissing: proc (self, start: NimNode): NimNode,
  leaves: seq[NimNode],
  index: int,
): NimNode =
  if leaves.len == 0:
    assert not kind.isNil
    let n = onTokenFound(self, kind, start)
    return quote do:
      discard `self`.nextChar
      `n`

  type Branch = tuple[kind: NimNode, leaves: seq[NimNode]]
  var branches = initTable[char, Branch]()

  for leaf in leaves:
    let
      raw = leaf[0].strVal
      ch = raw[index]

    if not branches.hasKey ch:
      branches[ch] = (nil, @[])

    if raw.len == index + 1:
      assert branches[ch].kind.isNil, fmt"duplicate '{raw}'"
      branches[ch].kind = leaf[1]
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
          branch.kind,
          start,
          onTokenFound,
          onTokenMissing,
          branch.leaves,
          index + 1,
        )

  result.add:
    newNimNode(nnkElse).add:
      if kind.isNil:
        onTokenMissing(self, start)
      else:
        onTokenFound(self, kind, start)

macro genCheckOp(
  self: Lexer,
  index: int,
  root: (string, TokenKind),
  leaves: varargs[(string, TokenKind)],
) =
  let index = cast[int](index.intVal)
  assert root[0].strVal.len == index
  for leaf in leaves:
    assert leaf[0].strVal.startsWith(root[0].strVal)

  proc onTokenFound(self, tokenKind, _: NimNode): NimNode =
    quote do: `self`.token.kind = `tokenKind`

  proc onTokenMissing(self, _: NimNode): NimNode =
    quote do: `self`.unknownToken

  newTokenCase(
    self,
    root[1],
    nil,
    onTokenFound,
    onTokenMissing,
    leaves[0 .. ^1],
    index,
  )

macro genCheckIdentOrKeyword(
  self: Lexer,
  start: int,
  leaves: varargs[(string, Keyword)],
) =
  proc onTokenFound(self, keyword, start: NimNode): NimNode =
    quote do:
      if `self`.currentChar.isIdentPartOrEnd:
        `self`.scanIdent(`start`)
      else:
        `self`.token.kind = tIdent
        `self`.token.value = TokenValue(
          kind: tvKeyword,
          keyword: `keyword`,
        )

  proc onTokenMissing(self, start: NimNode): NimNode =
    quote do: `self`.scanIdent(`start`)

  newTokenCase(
    self,
    nil,
    start,
    onTokenFound,
    onTokenMissing,
    leaves[0 .. ^1],
    index = 1,
  )

method nextToken(self: Lexer) =
  # Check previous token:
  if self.token.kind == tNewline or self.token.kind == tEof:
    # 1) After a newline or at the start of the stream (:EOF), a following comment can be a doc comment
    self.commentIsDoc = true
  elif self.token.kind != tSpace:
    # 2) Any non-space token prevents a following comment from being a doc
    # comment.
    self.commentIsDoc = false

  self.resetToken

  # Skip comments
  while self.currentChar == '#':
    let start = self.currentPos

    # TODO: Check #<loc:...> pragma comment
    if self.docEnabled and self.commentIsDoc:
      self.consumeDoc
    elif self.commentsEnabled:
      self.consumeComment(start)
      return
    else:
      self.skipComment

  var start = self.currentPos

  # TODO: implement macroExpansionPragmas

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
      self.`raise`"expected '\\n' after '\\r'"
  of '=':
    self.genCheckOp(
      index = 1,
      ("=", tOpEq),
      ("=>", tOpEqGt),
      ("=~", tOpEqTilde),
      ("==", tOpEqEq),
      ("===", tOpEqEqEq),
    )
  of '!':
    self.genCheckOp(
      index = 1,
      ("!", tOpBang),
      ("!=", tOpBangEq),
      ("!~", tOpBangTilde),
    )
  of '<':
    case self.nextChar
    of '=':
      self.genCheckOp(
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
    self.genCheckOp(
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
      self.`raise`"postfix increment is not supported, use `exp += 1`"
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
      self.`raise`"postfix decrement is not supported, use `exp -= 1`"
    else:
      self.token.kind = tOpMinus
  of '*':
    self.genCheckOp(
      index = 1,
      ("*", tOpStar),
      ("*=", tOpStarEq),
      ("**", tOpStarStar),
      ("**=", tOpStarStarEq),
    )
  of '/':
    let c = self.nextChar
    if (self.wantsDefOrMacroName or not self.slashIsRegex) and c == '/':
      self.genCheckOp(
        index = 2,
        ("//", tOpSlashSlash),
        ("//=", tOpSlashSlashEq),
      )
    elif not self.slashIsRegex and c == '=':
      self.nextChar tOpSlashEq
    elif self.wantsDefOrMacroName:
      self.token.kind = tOpSlash
    elif self.slashIsRegex:
      self.delimitedPair dkRegex, '/', '/', start, advance = false
    elif c.isSpaceAscii or c == '\0':
      self.token.kind = tOpSlash
    elif self.wantsRegex:
      self.delimitedPair dkRegex, '/', '/', start, advance = false
    else:
      self.token.kind = tOpSlash
  of '%':
    if self.wantsDefOrMacroName:
      self.nextChar tOpPercent
    else:
      case self.nextChar
      of '=':
        self.nextChar tOpPercentEq
      of '(', '[', '{', '<', '|':
        let c = self.currentChar
        self.delimitedPair dkString, c, c.closingChar, start
      of 'i':
        case self.peekNextChar
        of '(', '{', '[', '<', '|':
          let c = self.nextChar
          self.nextChar tSymbolArrayStart
          if self.wantsRaw:
            self.token.raw = fmt"%i{c}"
          self.token.delimiterState = newDelimiterState(dkSymbolArray, c, c.closingChar)
        else:
          self.token.kind = tOpPercent
      of 'q':
        case self.peekNextChar
        of '(', '{', '[', '<', '|':
          let c = self.nextChar
          self.delimitedPair dkString, c, c.closingChar, start, allowEscapes = false
        else:
          self.token.kind = tOpPercent
      of 'Q':
        case self.peekNextChar
        of '(', '{', '[', '<', '|':
          let c = self.nextChar
          self.delimitedPair dkString, c, c.closingChar, start
        else:
          self.token.kind = tOpPercent
      of 'r':
        case self.nextChar
        of '(', '{', '[', '<', '|':
          let c = self.currentChar
          self.delimitedPair dkString, c, c.closingChar, start
        else:
          self.`raise`"unknown %r char"
      of 'x':
        case self.nextChar
        of '(', '{', '[', '<', '|':
          let c = self.currentChar
          self.delimitedPair dkCommand, c, c.closingChar, start
        else:
          self.`raise`"unknown %x char"
      of 'w':
        case self.peekNextChar
        of '(', '{', '[', '<', '|':
          let c = self.nextChar
          self.nextChar tStringArrayStart
          if self.wantsRaw:
            self.token.raw = fmt"%w{c}"
          self.token.delimiterState = newDelimiterState(dkStringArray, c, c.closingChar)
        else:
          self.token.kind = tOpPercent
      of '}':
        self.nextChar tOpPercentRcurly
      else:
        self.token.kind = tOpPercent
  of '(': self.nextChar tOpLparen
  of ')': self.nextChar tOpRParen
  of '{':
    self.genCheckOp(
      index = 1,
      ("{", tOpLcurly),
      ("{%", tOpLcurlyPercent),
      ("{{", tOpLcurlyLcurly),
    )
  of '}': self.nextChar tOpRcurly
  of '[':
    self.genCheckOp(
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
  of ':':
    if self.nextChar == ':':
      self.nextChar tOpColonColon
    elif self.wantsSymbol:
      # TODO: implement consumeSymbol
      self.unknownToken
    else:
      self.token.kind = tOpColon
  of '~':
    self.nextChar tOpTilde
  of '.':
    let
      line = self.lineNumber
      column = self.columnNumber
    case self.nextChar
    of '.':
      case self.nextChar
      of '.':
        self.nextChar tOpPeriodPeriodPeriod
      else:
        self.token.kind = tOpPeriodPeriod
    else:
      if self.currentChar.isDigit:
        self.`raise`".1 style number literal is not supported, put 0 before dot", line, column
      self.token.kind = tOpPeriod
  of '&':
    case self.nextChar
    of '&':
      self.genCheckOp(
        index = 2,
        ("&&", tOpAmpAmp),
        ("&&=", tOpAmpAmpEq),
      )
    of '=':
      self.nextChar tOpAmpEq
    of '+':
      self.genCheckOp(
        index = 2,
        ("&+", tOpAmpPlus),
        ("&+=", tOpAmpPlusEq),
      )
    of '-':
      # Check if '>' comes after '&-', making it '&->'.
      # We want to parse that like '&(->...)',
      # so we only return '&' for now.
      if self.peekNextChar == '>':
        self.token.kind = tOpAmp
      else:
        self.genCheckOp(
          index = 2,
          ("&-", tOpAmpMinus),
          ("&-=", tOpAmpMinusEq),
        )
    of '*':
      self.genCheckOp(
        index = 2,
        ("&*", tOpAmpStar),
        ("&**", tOpAmpStarStar),
        ("&*=", tOpAmpStarEq),
      )
    else:
      self.token.kind = tOpAmp
  of '|':
    self.genCheckOp(
      index = 1,
      ("|", tOpBar),
      ("|=", tOpBarEq),
      ("||", tOpBarBar),
      ("||=", tOpBarBarEq),
    )
  of '^':
    self.genCheckOp(
      index = 1,
      ("^", tOpCaret),
      ("^=", tOpCaretEq),
    )
  of '\'':
    start = self.currentPos
    let
      line = self.lineNumber
      column = self.columnNumber
      char1 = self.nextChar
    self.token.kind = tChar
    case char1
    of '\\':
      let char2 = self.nextChar
      case char2
      of '\\':
        self.token.value = TokenValue(kind: tvChar, char: '\\')
      of '\'':
        self.token.value = TokenValue(kind: tvChar, char: '\'')
      of 'a':
        self.token.value = TokenValue(kind: tvChar, char: '\a')
      of 'b':
        self.token.value = TokenValue(kind: tvChar, char: '\b')
      of 'e':
        self.token.value = TokenValue(kind: tvChar, char: '\e')
      of 'f':
        self.token.value = TokenValue(kind: tvChar, char: '\f')
      of 'n':
        self.token.value = TokenValue(kind: tvChar, char: '\n')
      of 'r':
        self.token.value = TokenValue(kind: tvChar, char: '\r')
      of 't':
        self.token.value = TokenValue(kind: tvChar, char: '\t')
      of 'v':
        self.token.value = TokenValue(kind: tvChar, char: '\v')
      of 'u':
        # TODO: implement consumeCharUnicodeEscape
        self.unknownToken
      of '0':
        self.token.value = TokenValue(kind: tvChar, char: '\0')
      of '\0':
        self.`raise`"unterminated char literal", line, column
      else:
        self.`raise` &"invalid char escape sequence '\\{char2}'", line, column
    of '\'':
      self.`raise`"invalid empty char literal (did you mean '\\''?)", line, column
    of '\0':
      self.`raise`"unterminated char literal", line, column
    else:
      self.token.value = TokenValue(kind: tvChar, char: char1)
    if self.nextChar != '\'':
      self.`raise`"unterminated char literal, use double quotes for strings", line, column
    discard self.nextChar
    self.setTokenRawFromStart(start)
  of '`':
    if self.wantsDefOrMacroName:
      self.nextChar tOpGrave
    else:
      self.delimitedPair dkCommand, '`', '`', start
  of '"':
    self.delimitedPair dkString, '"', '"', start
  of '0'..'9':
    self.scanNumber start
  of '@':
    start = self.currentPos
    case self.nextChar
    of '[':
      self.nextChar tOpAtLsquare
    of '@':
      discard self.nextChar
      self.consumeVariable tClassVar, start
    else:
      self.consumeVariable tInstanceVar, start
  of '$':
    start = self.currentPos
    case self.nextChar
    of '~':
      self.nextChar tOpDollarTilde
    of '?':
      self.nextChar tOpDollarQuestion
    else:
      if self.currentChar.isDigit:
        start = self.currentPos
        if self.currentChar == '0':
          discard self.nextChar
        else:
          while self.nextChar.isDigit:
            discard
          if self.currentChar == '?':
            discard self.nextChar
        self.token.kind = tGlobalMatchDataIndex
        self.token.value = TokenValue(
          kind: tvString,
          string: self.stringRange(start),
        )
      else:
        self.consumeVariable tGlobal, start
  of 'a':
    self.genCheckIdentOrKeyword(
      start,
      ("abstract", kAbstract),
      ("alias", kAlias),
      ("asm", kAsm),
      ("as?", kAsQuestion),
      ("as", kAs),
      ("annotation", kAnnotation),
    )
  of 'b':
    self.genCheckIdentOrKeyword(
      start,
      ("begin", kBegin),
      ("break", kBreak),
    )
  of 'c':
    self.genCheckIdentOrKeyword(
      start,
      ("case", kCase),
      ("class", kClass),
    )
  of 'd':
    self.genCheckIdentOrKeyword(
      start,
      ("def", kDef),
      ("do", kDo),
    )
  of 'e':
    self.genCheckIdentOrKeyword(
      start,
      ("else", kElse),
      ("elsif", kElsif),
      ("end", kEnd),
      ("ensure", kEnsure),
      ("enum", kEnum),
      ("extend", kExtend),
    )
  of 'f':
    self.genCheckIdentOrKeyword(
      start,
      ("false", kFalse),
      ("for", kFor),
      ("fun", kFun),
    )
  of 'i':
    self.genCheckIdentOrKeyword(
      start,
      ("if", kIf),
      ("include", kInclude),
      ("instance_sizeof", kInstanceSizeof),
      ("in", kIn),
      ("is_a?", kIsAQuestion),
    )
  of 'l':
    self.genCheckIdentOrKeyword(
      start,
      ("lib", kLib),
    )
  of 'm':
    self.genCheckIdentOrKeyword(
      start,
      ("macro", kMacro),
      ("module", kModule),
    )
  of 'n':
    self.genCheckIdentOrKeyword(
      start,
      ("next", kNext),
      ("nil?", kNilQuestion),
      ("nil", kNil),
    )
  of 'o':
    self.genCheckIdentOrKeyword(
      start,
      ("offsetof", kOffsetof),
      ("of", kOf),
      ("out", kOut),
    )
  of 'p':
    self.genCheckIdentOrKeyword(
      start,
      ("pointerof", kPointerof),
      ("private", kPrivate),
      ("protected", kProtected),
    )
  of 'r':
    self.genCheckIdentOrKeyword(
      start,
      ("rescue", kRescue),
      ("responds_to?", kRespondsToQuestion),
      ("return", kReturn),
      ("require", kRequire),
    )
  of 's':
    self.genCheckIdentOrKeyword(
      start,
      ("select", kSelect),
      ("self", kSelf),
      ("sizeof", kSizeof),
      ("struct", kStruct),
      ("super", kSuper),
    )
  of 't':
    self.genCheckIdentOrKeyword(
      start,
      ("then", kThen),
      ("true", kTrue),
      ("typeof", kTypeof),
      ("type", kType),
    )
  of 'u':
    self.genCheckIdentOrKeyword(
      start,
      ("union", kUnion),
      ("uninitialized", kUninitialized),
      ("unless", kUnless),
      ("until", kUntil),
    )
  of 'v':
    self.genCheckIdentOrKeyword(
      start,
      ("verbatim", kVerbatim),
    )
  of 'w':
    self.genCheckIdentOrKeyword(
      start,
      ("when", kWhen),
      ("while", kWhile),
      ("with", kWith),
    )
  of 'y':
    self.genCheckIdentOrKeyword(
      start,
      ("yield", kYield),
    )
  of '_':
    case self.nextChar
    of '_':
      case self.nextChar
      of 'D':
        if self.charSequence('I', 'R', '_', '_'):
          if not self.peekNextChar.isIdentPartOrEnd:
            discard self.nextChar
            self.token.kind = tMagicDir
            return
      of 'E':
        if self.charSequence('N', 'D', '_', 'L', 'I', 'N', 'E', '_', '_'):
          if not self.peekNextChar.isIdentPartOrEnd:
            discard self.nextChar
            self.token.kind = tMagicEndLine
            return
      of 'F':
        if self.charSequence('I', 'L', 'E', '_', '_'):
          if not self.peekNextChar.isIdentPartOrEnd:
            discard self.nextChar
            self.token.kind = tMagicFile
            return
      of 'L':
        if self.charSequence('I', 'N', 'E', '_', '_'):
          if not self.peekNextChar.isIdentPartOrEnd:
            discard self.nextChar
            self.token.kind = tMagicLine
            return
      else:
        discard # scanIdent
    else:
      if not self.currentChar.isIdentPart:
        self.token.kind = tUnderscore
        return

    self.scanIdent(start)
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

proc lookahead*[T](
  self: Lexer,
  preserveTokenOnFail: bool,
  fun: proc (): Option[T] {.closure.},
): Option[T] =
  let
    oldPos = self.currentPos
    oldLine = self.lineNumber
    oldColumn = self.columnNumber
  if preserveTokenOnFail:
    self.tempToken.copyFrom(self.token)

  result = fun()
  if result.isNone:
    self.currentPos = oldPos
    self.lineNumber = oldLine
    self.columnNumber = oldColumn
    if preserveTokenOnFail:
      self.token.copyFrom(self.tempToken)

proc lookahead*[T](
  self: Lexer,
  fun: proc (): Option[T] {.closure.},
): Option[T] =
  self.lookahead(false, fun)

proc peekAhead*[T](self: Lexer, fun: proc (): T {.closure.}): T =
  var ret {.noInit.}: T
  discard self.lookahead(preserveTokenOnFail = true) do -> Option[void]:
    ret = fun()
    result = void.none
  ret

if isMainModule:
  proc assertLexes(
    s: string,
    tokenKind: TokenKind,
    setup: proc (lexer: Lexer) = nil,
  ) =
    let lexer = newLexer(s)
    if not setup.isNil:
      setup(lexer)
    lexer.nextToken
    assert lexer.token.kind == tokenKind
    case lexer.token.kind
    of {
      tClassVar, tConst,
      tGlobal,
      tIdent, tInstanceVar,
    }:
      assert lexer.token.value.kind == tvString
      assert lexer.token.value.string == s
    of tGlobalMatchDataIndex:
      assert lexer.token.value.kind == tvString
      assert lexer.token.value.string == s[1..^1]
    else:
      discard

  proc assertLexes(s: string, keyword: Keyword) =
    let lexer = newLexer(s)
    lexer.nextToken
    assert lexer.token.kind == tIdent
    assert lexer.token.value.kind == tvKeyword
    assert lexer.token.value.keyword == keyword

  proc assertLexes(
    s: string,
    kind: DelimiterKind,
    nest, `end`: char,
  ) =
    let lexer = newLexer(s)
    lexer.nextToken
    assert lexer.token.kind == tDelimiterStart
    assert lexer.token.delimiterState.kind == kind
    assert lexer.token.delimiterState.nestChar == nest
    assert lexer.token.delimiterState.endChar == `end`

  assertLexes("Foo", tConst)
  assertLexes("foo", tIdent)
  assertLexes("=", tOpEq)
  assertLexes("=>", tOpEqGt)
  assertLexes("==", tOpEqEq)
  assertLexes("===", tOpEqEqEq)
  assertLexes("123", tNumber)
  assertLexes("-123", tNumber)
  assertLexes("when", kWhen)
  assertLexes("w", tIdent)
  assertLexes("when?", tIdent)
  assertLexes("/", dkRegex, '/', '/')

  assertLexes("/", tOpSlash) do (lexer: Lexer):
    lexer.slashIsRegex = false

  assertLexes("//", tOpSlashSlash) do (lexer: Lexer):
    lexer.slashIsRegex = false

  assertLexes("//=", tOpSlashSlashEq) do (lexer: Lexer):
    lexer.slashIsRegex = false

  assertLexes("/=", tOpSlashEq) do (lexer: Lexer):
    lexer.slashIsRegex = false

  assertLexes("@foo", tInstanceVar)
  assertLexes("@@foo", tClassVar)
  assertLexes("$foo", tGlobal)
  assertLexes("$123", tGlobalMatchDataIndex)
  assertLexes("'e'", tChar)
