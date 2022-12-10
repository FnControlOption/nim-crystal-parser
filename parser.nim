{.push hint[XDeclaredButNotUsed]: off.}

import
  std/[options, sets, strformat, strutils],
  ./ast, ./lexer

type
  Unclosed = tuple[name: string, location: Location]

  Parser = ref object of Lexer
    visibility: Option[Visibility]
    defNest, funNest, typeNest: int
    wantsDoc: bool
    blockArgName: Option[string]

    varScopes: seq[HashSet[string]]
    unclosedStack: seq[Unclosed]
    callsSuper, callsInitialize, callsPreviousDef, usesBlockArg, isMacroDef, assignsSpecialVar, isConstantAssignment: bool
    callArgsStartLocations: seq[Location]
    tempArgCount: int
    inMacroExpression: bool
    stopOnYield: int
    insideCStruct: bool
    noTypeDeclaration: int
    consumingHeredocs, insideInterpolation: bool
    stopOnDo: bool
    assignedVars: seq[string]

    yields: Option[int]

proc init(
  self: Parser,
  s: string,
  # stringPool
  varScopes: seq[HashSet[string]],
  # warnings
) =
  self.Lexer.init(s)
  self.varScopes = varScopes
  self.unclosedStack = @[]
  self.callsSuper = false
  self.callsInitialize = false
  self.callsPreviousDef = false
  self.usesBlockArg = false
  self.isMacroDef = false
  self.assignsSpecialVar = false
  self.defNest = 0
  self.funNest = 0
  self.typeNest = 0
  self.isConstantAssignment = false

  self.callArgsStartLocations = @[]
  self.tempArgCount = 0
  self.inMacroExpression = false
  self.stopOnYield = 0
  self.insideCStruct = false
  self.wantsDoc = false
  self.docEnabled = false
  self.noTypeDeclaration = 0
  self.consumingHeredocs = false
  self.insideInterpolation = false

  self.stopOnDo = false
  self.assignedVars = @[]

proc new*(
  T: type Parser,
  s: string,
  # stringPool
  varScopes = @[initHashSet[string]()],
): Parser =
  new(result)
  result.init(s, varScopes)

proc isMultiAssignTarget(exp: ASTNode): bool =
  if exp of Underscore or
      exp of Var or
      exp of InstanceVar or
      exp of ClassVar or
      exp of Global or
      exp of Assign:
    result = true
  elif exp of Call:
    let call = exp.Call
    result = not call.hasParentheses and (
      (call.args.len == 0 and call.namedArgs.isNone) or
        call.name.isSetter or
        call.name in ["[]", "[]="]
    )
  else:
    result = false

proc isMultiAssignMiddle(exp: ASTNode): bool =
  if exp of Assign:
    result = true
  elif exp of Call:
    result = exp.Call.name.endsWith '='
  else:
    result = false

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

proc checkNotInsideDef[T](
  self: Parser,
  message: string,
  fun: proc (): T {.closure.},
): T =
  if self.defNest == 0 and self.funNest == 0:
    result = fun()
  else:
    let suffix = if self.isInsideDef: " inside def" else: " inside fun"
    self.`raise`message & suffix, self.token.lineNumber, self.token.columnNumber

template isInsideDef(self: Parser): bool =
  self.defNest > 0

template isInsideFun(self: Parser): bool =
  self.funNest > 0

proc callBlockArgFollows(self: Parser): bool =
  self.token.kind == tOpAmp and not self.currentChar.isSpaceAscii

proc isStatementEnd(self: Parser): bool =
  case self.token.kind
  of tNewline, tOpSemicolon:
    result = true
  else:
    result = self.token.isKeyword(kEnd)

proc checkNotPipeBeforeProcLiteralBody(self: Parser) =
  if self.token.kind == tOpBar:
    let location = self.token.location
    self.nextTokenSkipSpace
    var msg = "unexpected token: \"|\", proc literals specify their parameters like this: ->("
    if self.token.kind == tIdent:
      msg.add self.token.value.`$`
      msg.add " : Type"
      self.nextTokenSkipSpaceOrNewline
      if self.token.kind == tOpComma:
        msg.add ", ..."
    else:
      msg.add "param : Type"
    msg.add ") { ... }"
    self.`raise`msg, location

proc isNamedTupleStart(self: Parser): bool =
  case self.token.kind
  of tIdent, tConst:
    result = self.currentChar == ':' and self.peekNextChar != ':'
  else:
    result = false

proc isStringLiteralStart(self: Parser): bool =
  self.token.kind == tDelimiterStart and self.token.delimiterState.kind == dkString

proc checkValidDefName(self: Parser) =
  if self.token.value in {kIsAQuestion, kAs, kAsQuestion, kRespondsToQuestion, kNilQuestion}:
    self.`raise`fmt"'{self.token.value}' is a pseudo-method and can't be redefined", self.token

proc checkValidDefOpName(self: Parser) =
  if self.token.kind == tOpBang:
    self.`raise`"'!' is a pseudo-method and can't be redefined", self.token

proc computeBlockArgYields(self: Parser, blockArg: Arg) =
  let blockArgRestriction = blockArg.restriction.get(nil)
  if not blockArgRestriction.isNil and blockArgRestriction of ProcNotation:
    let inputs = blockArgRestriction.ProcNotation.inputs
    self.yields = some(if inputs.isSome: inputs.get.len else: 0)
  else:
    self.yields = 0.some

proc isInvalidInternalName(keyword: TokenValue): bool =
  case keyword.kind
  of tvKeyword:
    case keyword.keyword
    of kBegin, kNil, kTrue, kFalse, kYield, kWith, kAbstract,
       kDef, kMacro, kRequire, kCase, kSelect, kIf, kUnless, kInclude,
       kExtend, kClass, kStruct, kModule, kEnum, kWhile, kUntil, kReturn,
       kNext, kBreak, kLib, kFun, kAlias, kPointerof, kSizeof, kOffsetof,
       kInstanceSizeof, kTypeof, kPrivate, kProtected, kAsm, kOut,
       kSelf, kIn, kEnd:
      result = true
    else:
      result = false
  of tvString:
    case keyword.string
    of "begin", "nil", "true", "false", "yield", "with", "abstract",
       "def", "macro", "require", "case", "select", "if", "unless", "include",
       "extend", "class", "struct", "module", "enum", "while", "until", "return",
       "next", "break", "lib", "fun", "alias", "pointerof", "sizeof", "offsetof",
       "instance_sizeof", "typeof", "private", "protected", "asm", "out",
       "self", "in", "end":
      result = true
    else:
      result = false
  else:
    result = false

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
  newValue: bool,
  fun: proc (): T {.closure.},
): T =
  let oldStopOnDo = self.stopOnDo
  self.stopOnDo = newValue
  result = fun()
  self.stopOnDo = oldStopOnDo

proc preserveStopOnDo[T](
  self: Parser,
  fun: proc (): T {.closure.},
): T =
  result = self.preserveStopOnDo(false, fun)

proc makeNilableType(t: ASTNode): ASTNode =
  result = Union.new(@[t, Path.global("Nil").at(t)]).at(t)

proc makeNilableExpression(t: ASTNode): ASTNode =
  let t = Generic.new(
    Path.global("Union").at(t),
    @[t, Path.global("Nil").at(t)],
  ).at(t)
  t.suffix = gsQuestion
  result = t

proc makePointerType(t: ASTNode): ASTNode =
  let t = Generic.new(
    Path.global("Pointer").at(t),
    @[t],
  ).at(t)
  t.suffix = gsAsterisk
  result = t

proc makeStaticArrayType(t: ASTNode, size: ASTNode): ASTNode =
  let t = Generic.new(
    Path.global("StaticArray").at(t),
    @[t, size],
  ).at(t)
  t.suffix = gsBracket
  result = t

proc makeTupleType(types: seq[ASTNode]): ASTNode =
  Generic.new(Path.global("Tuple"), types)

proc makeNamedTupleType(
  namedArgs: Option[seq[NamedArgument]],
): ASTNode =
  Generic.new(
    Path.global("NamedTuple"),
    @[],
    namedArgs = namedArgs,
  )

proc delimiterOrTypeSuffix(self: Parser): bool =
  case self.token.kind
  of tOpPeriod:
    self.nextTokenSkipSpaceOrNewline
    result = self.token.isKeyword(kClass)
  of tOpQuestion, tOpStar, tOpStarStar:
    self.nextTokenSkipSpace
    result = self.delimiterOrTypeSuffix
  of tOpMinusGt, tOpBar, tOpComma, tOpEqGt, tNewline, tEof,
     tOpEq, tOpSemicolon, tOpLparen, tOpRparen, tOpLsquare, tOpRsquare:
    result = true
  else:
    result = false

proc typePathStart(self: Parser): bool =
  while self.token.kind == tConst:
    self.nextToken
    if self.token.kind != tOpColonColon:
      break
    self.nextTokenSkipSpaceOrNewline

  self.skipSpace
  result = self.delimiterOrTypeSuffix

proc typeStart(self: Parser): bool =
  while self.token.kind == tOpLparen or self.token.kind == tOpLcurly:
    self.nextTokenSkipSpaceOrNewline

  case self.token.kind
  of tIdent:
    if self.isNamedTupleStart:
      return false
    if self.token.value == kTypeof:
      result = true
    elif self.token.value == kSelf or self.token.value == "self?":
      self.nextTokenSkipSpace
      result = self.delimiterOrTypeSuffix
    else:
      result = false
  of tConst:
    if self.isNamedTupleStart:
      return false
    result = self.typePathStart
  of tOpColonColon:
    self.nextToken
    result = self.typePathStart
  of tUnderscore, tOpMinusGt:
    result = true
  of tOpStar:
    self.nextTokenSkipSpaceOrNewline
    result = self.typeStart
  else:
    result = false

proc isTypeStart(self: Parser, consumeNewlines: bool): bool =
  self.peekAhead do -> bool:
    try:
      if consumeNewlines:
        self.nextTokenSkipSpaceOrNewline
      else:
        self.nextTokenSkipSpace

      result = self.typeStart
    except:
      result = false

proc nextToken(self: Parser, node: ASTNode) =
  node.endLocation = self.tokenEndLocation
  self.nextToken

proc isEndToken(self: Parser): bool =
  case self.token.kind
  of tOpRcurly, tOpRsquare, tOpPercentRcurly, tEof:
    result = true
  else:
    if self.token.value in {kDo, kEnd, kElse, kElsif, kWhen, kIn, kRescue, kEnsure, kThen}:
      result = not self.nextComesColonSpace
    else:
      result = false

proc canBeAssigned(node: ASTNode): bool =
  if node of Var or
      node of InstanceVar or
      node of ClassVar or
      node of Path or
      node of Global or
      node of Underscore:
    result = true
  elif node of Call:
    let call = node.Call
    result = (call.obj.isNone and call.args.len == 0 and call.`block`.isNone) or call.name == "[]"
  else:
    result = false

proc checkVoidValue(
  self: Parser,
  exp: ASTNode,
  location: Location,
) =
  if exp of ControlExpression:
    self.`raise`"void value expression", location

proc checkVoidExpressionKeyword(self: Parser) =
  if self.token.value in {kBreak, kNext, kReturn}:
    if not self.nextComesColonSpace:
      self.`raise`"void value expression", self.token, self.token.value.`$`.len

proc check(self: Parser, tokenKinds: openArray[TokenKind]) =
  if self.token.kind notin tokenKinds:
    let tokenKinds = tokenKinds.join ", "
    self.`raise`fmt"expecting any of these tokens: '{tokenKinds}' (not '{self.token.kind}')", self.token

proc check(self: Parser, tokenKind: TokenKind) =
  if self.token.kind != tokenKind:
    self.`raise`fmt"expecting token '{tokenKind}', not '{self.token}'", self.token

proc checkIdent(self: Parser, value: Keyword) =
  if not self.token.isKeyword(value):
    self.`raise`fmt"expecting identifier '{value}', not '{self.token}'", self.token

const DefOrMacroCheck1 = [
  tIdent, tConst, tOpGrave,
  tOpLtLt, tOpLt, tOpLtEq, tOpEqEq, tOpEqEqEq, tOpBangEq, tOpEqTilde,
  tOpBangTilde, tOpGtGt, tOpGt, tOpGtEq, tOpPlus, tOpMinus, tOpStar, tOpSlash,
  tOpSlashSlash, tOpBang, tOpTilde, tOpPercent, tOpAmp, tOpBar, tOpCaret, tOpStarStar,
  tOpLsquareRsquare, tOpLsquareRsquareEq, tOpLsquareRsquareQuestion, tOpLtEqGt,
  tOpAmpPlus, tOpAmpMinus, tOpAmpStar, tOpAmpStarStar,
]

proc consumeDefOrMacroName(self: Parser): string =
  self.wantsDefOrMacroName = true
  self.nextTokenSkipSpaceOrNewline
  self.check DefOrMacroCheck1
  self.wantsDefOrMacroName = false
  $self.token

proc consumeDefEqualsSignSkipSpace(self: Parser): bool =
  self.nextToken
  if self.token.kind == tOpEq:
    self.nextTokenSkipSpace
    result = true
  else:
    self.skipSpace
    result = false

proc withIsolatedVarScope[T](
  self: Parser,
  createScope: bool,
  fun: proc (): T {.closure.},
): T =
  if not createScope:
    return fun()

  self.varScopes.add(initHashSet[string]())
  result = fun()
  discard self.varScopes.pop

proc withIsolatedVarScope[T](
  self: Parser,
  fun: proc (): T {.closure.},
): T =
  self.withIsolatedVarScope(true, fun)

proc withLexicalVarScope[T](
  self: Parser,
  fun: proc (): T {.closure.},
): T =
  var currentScope = initHashSet[string]()
  for name in self.varScopes[^1]:
    currentScope.incl(name)
  self.varScopes.add currentScope
  result = fun()
  discard self.varScopes.pop

proc pushVarName(self: Parser, name: string) =
  self.varScopes[^1].incl name

proc pushVar(self: Parser, node: ASTNode) =
  if node of Var:
    self.pushVarName node.Var.name
  elif node of Arg:
    self.pushVarName node.Arg.name
  elif node of TypeDeclaration:
    let v = node.TypeDeclaration.`var`
    if v of Var:
      self.pushVarName v.Var.name
    elif v of InstanceVar:
      self.pushVarName v.InstanceVar.name
    else:
      self.`raise`"can't happen"
  else:
    discard

proc pushVars(self: Parser, vars: seq[ASTNode]) =
  for v in vars:
    self.pushVar v

proc isVarInScope(self: Parser, name: string): bool =
  result = self.varScopes[^1].contains name

proc open[T](
  self: Parser,
  symbol: string,
  location: Location,
  fun: proc (): T {.closure.},
): T =
  self.unclosedStack.add (symbol, location)
  result = fun()
  discard self.unclosedStack.pop

proc open[T](
  self: Parser,
  symbol: string,
  fun: proc (): T {.closure.},
): T =
  self.open(symbol, self.token.location, fun)

proc checkIdent(self: Parser): string =
  self.check tIdent
  result = $self.token.value

proc checkConst(self: Parser): string =
  self.check tConst
  result = $self.token.value

proc unexpectedToken(
  self: Parser,
  msg = string.none,
  token = self.token,
) {.noreturn.} =
  let tokenStr = if token.kind == tEof: "EOF" else: token.`$`.escape
  if msg.isSome:
    self.`raise`fmt"unexpected token: {token_str} ({msg.get})", token
  else:
    self.`raise`fmt"unexpected token: {token_str}", token

proc unexpectedTokenInAtomic(self: Parser) {.noreturn.} =
  if self.unclosedStack.len > 0:
    let unclosed = self.unclosedStack[^1]
    self.`raise`fmt"unterminated {unclosed.name}", unclosed.location

  self.unexpectedToken

proc isVar(self: Parser, name: string): bool =
  if self.inMacroExpression:
    return true

  result = name == "self" or self.isVarInScope(name)

proc pushVisibility[T](
  self: Parser,
  fun: proc (): T {.closure.},
): T =
  let oldVisibility = self.visibility
  self.visibility = Visibility.none
  result = fun()
  self.visibility = oldVisibility

method nextToken(self: Parser) =
  # TODO: implement consumeHeredocs
  procCall nextToken(Lexer self)

proc tempArgName(self: Parser): string =
  result = fmt"__arg{self.tempArgCount}"
  self.tempArgCount += 1

proc parseVarOrCall(self: Parser): ASTNode =
  # TODO: implement
  result = Call.new(ASTNode.none, self.token.value.`$`)
  self.nextToken

proc parseAtomicWithoutLocation(self: Parser): ASTNode =
  case self.token.kind
  # TODO
  of tNumber:
    self.wantsRegex = false
    result = NumberLiteral.new(self.token.value.string, self.token.numberKind)
    self.nextToken result
  of tChar:
    result = CharLiteral.new(self.token.value.char)
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

proc parseOpAssign(
  self: Parser,
  allowOps, allowSuffix = true,
): ASTNode =
  let
    doc = self.token.doc
    location = self.token.location
    startToken = self.token

  result = self.parseQuestionColon

  var allowOps = allowOps
  while true:
    let nameLocation = self.token.location

    case self.token.kind
    of tSpace:
      self.nextToken
      continue
    of tIdent:
      if not allowSuffix:
        self.unexpectedToken
      break
    of tOpEq:
      self.slashIsRegex = true
      # TODO: implement
      self.unexpectedToken
    else:
      if not self.token.kind.isAssignmentOperator:
        break
      # TODO: implement
      self.unexpectedToken
    allowOps = true

proc parseOpAssignNoControl(
  self: Parser,
  allowOps, allowSuffix = true,
): ASTNode =
  self.checkVoidExpressionKeyword
  result = self.parseOpAssign(allowOps, allowSuffix)

proc parseExpressionSuffix(
  self: Parser,
  location: Location,
  fun: proc (exp: ASTNode): ASTNode {.closure.},
): ASTNode =
  self.slashIsRegex = true
  self.nextTokenSkipStatementEnd
  result = self.parseOpAssignNoControl
  result = fun(result).at(location).atEnd(result)

proc parseExpression(self: Parser): ASTNode =
  # TODO: implement
  result = self.parseOpAssign

proc parseMultiAssign(self: Parser): ASTNode =
  let location = self.token.location

  var lhsSplatIndex = int.none
  if self.token.kind == tOpStar:
    lhsSplatIndex = 0.some
    self.nextTokenSkipSpace

  var last = self.parseExpression
  self.skipSpace

  case self.token.kind
  of tOpComma:
    if not last.isMultiAssignTarget:
      if lhsSplatIndex.isSome:
        self.unexpectedToken
      if last of Path:
        self.`raise`"Multiple assignment is not allowed for constants"
      self.unexpectedToken
  of tNewline, tOpSemicolon:
    if lhsSplatIndex.isSome and not last.isMultiAssignMiddle:
      self.unexpectedToken
    if lhsSplatIndex.isNone:
      return last
  else:
    if self.isEndToken:
      if lhsSplatIndex.isSome and not last.isMultiAssignMiddle:
        self.unexpectedToken
      if lhsSplatIndex.isNone:
        return last
    else:
      self.unexpectedToken

  var
    exps = @[last]
    i = 0
    assignIndex = -1

  while self.token.kind == tOpComma:
    if assignIndex == -1 and last.isMultiAssignMiddle:
      assignIndex = i

    i += 1

    self.nextTokenSkipSpaceOrNewline
    if self.token.kind == tOpStar:
      if lhsSplatIndex.isSome:
        self.`raise`"splat assignment already specified"
      lhsSplatIndex = i.some
      self.nextTokenSkipSpace

    last = self.parseOpAssign(allowOps = false)
    if assignIndex == -1 and not last.isMultiAssignTarget:
      self.unexpectedToken

    exps.add last
    self.skipSpace

  # TODO: implement
  self.unexpectedToken

proc parseExpressionsInternal(self: Parser): ASTNode =
  if self.isEndToken:
    return Nop.new

  result = self.parseMultiAssign

  self.slashIsRegex = true
  self.skipStatementEnd

  if self.isEndToken:
    return

  var exps = @[result]

  while true:
    exps.add self.parseMultiAssign
    self.skipStatementEnd
    if self.isEndToken:
      break

  result = Expressions.from(exps)

proc parseExpressions(self: Parser): ASTNode =
  self.preserveStopOnDo do -> ASTNode:
    result = self.parseExpressionsInternal

proc parse*(self: Parser): ASTNode =
  self.nextTokenSkipStatementEnd
  result = self.parseExpressions
  self.check tEof

proc parse*(
  s: string,
  # stringPool
  varScopes = @[initHashSet[string]()],
): ASTNode =
  Parser.new(s, varScopes).parse

if isMainModule:
  var node = parse("")
  assert node of Nop

  node = parse("foo")
  discard node of Call

  node = parse("'e'")
  assert node of CharLiteral

  node = parse("123")
  assert node of NumberLiteral
