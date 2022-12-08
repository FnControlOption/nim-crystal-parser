import ast, lexer, options, token
import std/strformat, std/strutils

type Parser = ref object of Lexer
  stopOnDo: bool
  visibility: Option[Visibility]
  noTypeDeclaration: int

proc initParser(
  self: Parser,
  s: string,
  # stringPool
  # varScopes
  # warnings
) =
  self.initLexer(s)
  # TODO: implement

proc newParser*(s: string): Parser =
  new(result)
  result.initParser(s)

proc unexpectedToken(
  self: Parser,
  msg = string.none,
  token = self.token,
) {.noReturn.} =
  let tokenStr = if token.kind == tEof: "EOF" else: token.`$`.escape
  if msg.isSome:
    self.`raise` fmt"unexpected token: {token_str} ({msg.get})", token
  else:
    self.`raise` fmt"unexpected token: {token_str}", token

proc preserveStopOnDo[T](
  self: Parser,
  newValue: bool,
  fun: proc (self: Parser): T,
): T =
  let oldStopOnDo = self.stopOnDo
  self.stopOnDo = newValue
  result = fun(self)
  self.stopOnDo = oldStopOnDo

proc setVisibility[T: ASTNode](self: Parser, node: T) =
  if self.visibility.isSome:
    node.visibility = self.visibility.get

proc nextComesPlusOrMinus(self: Parser): bool =
  let pos = self.currentPos
  while self.currentChar.isSpaceAscii:
    discard self.nextCharNoColumnIncrement
  result = self.currentChar in {'+', '-'}
  self.currentPos = pos

proc preserveStopOnDo[T](
  self: Parser,
  fun: proc (self: Parser): T,
): T =
  result = self.preserveStopOnDo(false, fun)

proc check(self: Parser, tokenKind: TokenKind) =
  if tokenKind != self.token.kind:
    self.`raise` fmt"expecting token '{tokenKind}', not '{self.token}'", self.token

proc nextComesColonSpace(self: Parser): bool =
  if self.noTypeDeclaration != 0:
    return false

  let pos = self.currentPos
  while self.currentChar.isSpaceAscii:
    discard self.nextCharNoColumnIncrement
  result = self.currentChar == ':'
  if result:
    discard self.nextCharNoColumnIncrement
    result = self.currentChar.isSpaceAscii
  self.currentPos = pos

proc isEndToken(self: Parser): bool =
  case self.token.kind
  of tOpRcurly, tOpRsquare, tOpPercentRcurly, tEof:
    result = true
  else:
    if self.token.value.kind == tvKeyword:
      case self.token.value.keyword
      of kDo, kEnd, kElse, kElsif, kWhen, kIn, kRescue, kEnsure, kThen:
        result = not self.nextComesColonSpace
      else:
        result = false
    else:
      result = false

method nextToken(self: Parser) =
  # TODO: implement consumeHeredocs
  procCall nextToken(Lexer self)

proc nextToken(self: Parser, node: ASTNode) =
  node.endLocation = self.tokenEndLocation
  self.nextToken

proc parseVarOrCall(self: Parser): ASTNode =
  # TODO: implement
  result = newCall(ASTNode.none, self.token.value.`$`)
  self.nextToken

proc parseAtomicWithoutLocation(self: Parser): ASTNode =
  case self.token.kind
  # TODO
  of tNumber:
    self.wantsRegex = false
    result = newNumberLiteral(self.token.value.string, self.token.numberKind)
    self.nextToken result
  of tChar:
    result = newCharLiteral(self.token.value.char)
    self.nextToken result
  # TODO
  of tIdent:
    if self.token.value.kind == tvKeyword:
      # TODO: implement
      self.unexpectedToken
    else:
      result = self.parseVarOrCall
      self.setVisibility result
  # TODO
  else:
    self.unexpectedToken

proc parseAtomic(self: Parser): ASTNode =
  # TODO: implement
  result = self.parseAtomicWithoutLocation

proc parseAtomicWithMethod(self: Parser): ASTNode =
  # TODO: implement
  result = self.parseAtomic

proc parsePrefix(self: Parser): ASTNode =
  # TODO: implement
  result = self.parseAtomicWithMethod

proc parseRange(self: Parser): ASTNode =
  # TODO: implement
  result = self.parsePrefix

proc parseQuestionColon(self: Parser): ASTNode =
  # TODO: implement
  result = self.parseRange

proc parseOpAssign(self: Parser): ASTNode =
  # TODO: implement
  result = self.parseQuestionColon

proc parseExpression(self: Parser): ASTNode =
  # TODO: implement
  result = self.parseOpAssign

proc parseMultiAssign(self: Parser): ASTNode =
  # TODO: implement
  result = self.parseExpression

proc parseExpressionsInternal(self: Parser): ASTNode =
  if self.isEndToken:
    return newNop()

  # TODO: implement
  result = self.parseMultiAssign

proc parseExpressions(self: Parser): ASTNode =
  self.preserveStopOnDo do (self: Parser) -> ASTNode:
    result = self.parseExpressionsInternal

proc parse*(self: Parser): ASTNode =
  self.nextTokenSkipStatementEnd
  result = self.parseExpressions
  self.check tEof

proc parse*(
  s: string,
  # stringPool
  # varScopes
): ASTNode =
  newParser(s).parse

if isMainModule:
  var node = parse("")
  assert node of Nop

  node = parse("foo")
  discard node of Call

  node = parse("'e'")
  assert node of CharLiteral

  node = parse("123")
  assert node of NumberLiteral
