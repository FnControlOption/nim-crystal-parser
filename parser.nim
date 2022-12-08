import ast, lexer, options, token
import std/strformat, std/strutils

type Parser = ref object of Lexer
  stopOnDo: bool

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
  blk: proc (self: Parser): T,
): T =
  let oldStopOnDo = self.stopOnDo
  self.stopOnDo = newValue
  result = blk(self)
  self.stopOnDo = oldStopOnDo

proc preserveStopOnDo[T](
  self: Parser,
  blk: proc (self: Parser): T,
): T =
  result = self.preserveStopOnDo(false, blk)

proc check(self: Parser, tokenKind: TokenKind) =
  if tokenKind != self.token.kind:
    self.`raise` fmt"expecting token '{tokenKind}', not '{self.token}'", self.token

proc parseExpressionsInternal(self: Parser): ASTNode =
  # TODO: implement
  result = newNop()

proc parseExpressions(self: Parser): ASTNode =
  self.preserveStopOnDo do (self: Parser) -> ASTNode:
    result = self.parseExpressionsInternal

proc parse(self: Parser): ASTNode =
  self.nextTokenSkipStatementEnd
  result = self.parseExpressions
  self.check tEof

if isMainModule:
  let parser = newParser("foo")
  # discard parser.parse
