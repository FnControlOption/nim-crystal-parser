import location, options

type
  ASTNode* = ref object of RootObj
    privateLocation, privateEndLocation: Option[Location]

  Nop* = ref object of ASTNode

  ExpressionsKeyword* = enum
    ekNone
    ekParen
    ekBegin

  Expressions* = ref object of ASTNode
    expressions: seq[ASTNode]
    keyword: ExpressionsKeyword

  NilLiteral* = ref object of ASTNode

  BoolLiteral* = ref object of ASTNode
    value: bool

  NumberKind* = enum
    nkI8
    nkI16
    nkI32
    nkI64
    nkI128
    nkU8
    nkU16
    nkU32
    nkU64
    nkU128
    nkF32
    nkF64

  NumberLiteral* = ref object of ASTNode
    value: string
    kind: NumberKind

  CharLiteral* = ref object of ASTNode
    value: char

  StringLiteral* = ref object of ASTNode
    value: string

  StringInterpolation* = ref object of ASTNode
    expressions: seq[ASTNode]

  SymbolLiteral* = ref object of ASTNode
    value: string

  ArrayLiteral* = ref object of ASTNode
    elements: seq[ASTNode]
    `of`, name: Option[ASTNode]

  HashLiteral* = ref object of ASTNode
    entries: seq[HashEntry]
    `of`: Option[HashEntry]
    name: Option[ASTNode]

  HashEntry* = object
    key, value: ASTNode

  NamedTupleLiteral* = ref object of ASTNode
    entries: seq[NamedTupleEntry]

  NamedTupleEntry* = object
    key: string
    value: ASTNode

  RangeLiteral* = ref object of ASTNode
    `from`, to: ASTNode
    exclusive: bool

  RegexLiteral* = ref object of ASTNode
    value: ASTNode
    # options

  TupleLiteral* = ref object of ASTNode
    elements: seq[ASTNode]

  # SpecialVar

  Var* = ref object of ASTNode
    name: string

  Block* = ref object of ASTNode
    args: seq[Var]
    body: ASTNode
    call: Option[Call]
    splatIndex: Option[int]

  Call* = ref object of ASTNode
    obj: Option[ASTNode]
    name: string
    args: seq[ASTNode]
    `block`: Option[Block]
    blockArg: Option[ASTNode]
    namedArgs: Option[seq[NamedArgument]]
    nameLocation: Option[Location]
    nameSize: int
    doc: Option[string]
    visibility: Visibility
    global, expansion, hasParentheses: bool

  NamedArgument* = ref object of ASTNode
    name: string
    value: ASTNode

  If* = ref object of ASTNode
    cond, then, `else`: ASTNode
    ternary: bool
    elseLocation: Option[Location]

  Unless* = ref object of ASTNode
    cond, then, `else`: ASTNode
    elseLocation: Option[Location]

  Assign* = ref object of ASTNode
    target, value: ASTNode
    doc: Option[string]

  OpAssign* = ref object of ASTNode
    target: ASTNode
    op: string
    value: ASTNode
    nameLocation: Option[Location]

  MultiAssign* = ref object of ASTNode
    targets, values: seq[ASTNode]

  InstanceVar* = ref object of ASTNode
    name: string

  ReadInstanceVar* = ref object of ASTNode
    obj: ASTNode
    name: string

  ClassVar* = ref object of ASTNode
    name: string

  Global* = ref object of ASTNode
    name: string

  BinaryOp* = ref object of ASTNode
    left, right: ASTNode

  And* = ref object of BinaryOp

  Or* = ref object of BinaryOp

  Arg* = ref object of ASTNode
    name, externalName: string
    defaultValue, restriction: Option[ASTNode]
    doc: Option[string]
    parsedAnnotations: Option[seq[Annotation]]

  ProcNotation* = ref object of ASTNode
    inputs: Option[seq[ASTNode]]
    output: Option[ASTNode]

  Def* = ref object of ASTNode
    freeVars: Option[seq[string]]
    receiver: Option[ASTNode]
    name: string
    args: seq[Arg]
    doubleSplat: Option[Arg]
    body: ASTNode
    blockArg: Option[Arg]
    returnType: Option[ASTNode]
    yields: Option[int]
    nameLocation: Option[Location]
    splatIndex: Option[int]
    doc: Option[string]
    visibility: Visibility

    macroDef: bool
    callsSuper: bool
    callsInitialize: bool
    callsPreviousDef: bool
    usesBlockArg: bool
    assignsSpecialVar: bool
    abstract: bool

  Macro* = ref object of ASTNode
    name: string
    args: seq[Arg]
    body: ASTNode
    doubleSplat, blockArg: Option[Arg]
    nameLocation: Option[Location]
    splatIndex: Option[int]
    doc: Option[string]
    visibility: Visibility

  UnaryExpression* = ref object of ASTNode
    exp: ASTNode

  Not* = ref object of UnaryExpression

  PointerOf* = ref object of UnaryExpression

  SizeOf* = ref object of UnaryExpression

  InstanceSizeOf* = ref object of UnaryExpression

  Out* = ref object of UnaryExpression

  OffsetOf* = ref object of ASTNode
    offsetofType, offset: ASTNode

  VisibilityModifier* = ref object of ASTNode
    modifier: Visibility
    exp: ASTNode
    doc: Option[string]

  IsA* = ref object of ASTNode
    obj, `const`: ASTNode
    nilCheck: bool

  RespondsTo* = ref object of ASTNode
    obj: ASTNode
    name: string

  Require* = ref object of ASTNode
    string: string

  When* = ref object of ASTNode
    conds: seq[ASTNode]
    body: ASTNode
    exhaustive: bool

  Case* = ref object of ASTNode
    cond: Option[ASTNode]
    whens: seq[When]
    `else`: Option[ASTNode]
    exhaustive: bool

  SelectWhen* = object
    condition, body: ASTNode

  Select* = ref object of ASTNode
    whens: seq[SelectWhen]
    `else`: Option[ASTNode]

  ImplicitObj* = ref object of ASTNode

  Path* = ref object of ASTNode
    names: seq[string]
    global: bool
    visibility: Visibility

  ClassDef* = ref object of ASTNode
    name: Path
    body: ASTNode
    superclass: Option[ASTNode]
    typeVars: Option[seq[string]]
    nameLocation: Option[Location]
    doc: Option[string]
    splatIndex: Option[int]
    abstract: bool
    struct: bool
    visibility: Visibility

  ModuleDef* = ref object of ASTNode
    name: Path
    body: ASTNode
    typeVars: Option[seq[string]]
    splatIndex: Option[int]
    nameLocation: Option[Location]
    doc: Option[string]
    visibility: Visibility

  AnnotationDef* = ref object of ASTNode
    name: Path
    doc: Option[string]
    nameLocation: Option[Location]

  While* = ref object of ASTNode
    cond, body: ASTNode

  Until* = ref object of ASTNode
    cond, body: ASTNode

  Generic* = ref object of ASTNode
    name: ASTNode
    typeVars: seq[ASTNode]
    namedArgs: Option[seq[NamedArgument]]

  TypeDeclaration* = ref object of ASTNode
    `var`, declaredType: ASTNode
    value: Option[ASTNode]

  UninitializedVar* = ref object of ASTNode
    `var`, declaredType: ASTNode

  Rescue* = ref object of ASTNode
    body: ASTNode
    types: Option[seq[ASTNode]]
    name: Option[string]

  ExceptionHandler* = ref object of ASTNode
    body: ASTNode
    rescues: Option[seq[Rescue]]
    `else`, ensure: Option[ASTNode]
    implicit, suffix: bool
    elseLocation, ensureLocation: Option[Location]

  ProcLiteral* = ref object of ASTNode
    def: Def

  ProcPointer* = ref object of ASTNode
    obj: Option[ASTNode]
    name: string
    args: seq[ASTNode]
    global: bool

  Union* = ref object of ASTNode
    types: seq[ASTNode]

  Self* = ref object of ASTNode

  ControlExpression* = ref object of ASTNode
    exp: Option[ASTNode]

  Return* = ref object of ControlExpression

  Break* = ref object of ControlExpression

  Next* = ref object of ControlExpression

  Yield* = ref object of ASTNode
    exps: seq[ASTNode]
    scope: Option[ASTNode]
    hasParentheses: bool

  Include* = ref object of ASTNode
    name: ASTNode

  Extend* = ref object of ASTNode
    name: ASTNode

  LibDef* = ref object of ASTNode
    name: string
    body: ASTNode
    nameLocation: Option[Location]
    visibility: Visibility

  FunDef* = ref object of ASTNode
    name: string
    args: seq[Arg]
    returnType, body: Option[ASTNode]
    realName: string
    doc: Option[string]
    `varargs`: bool

  TypeDef* = ref object of ASTNode
    name: string
    typeSpec: ASTNode
    nameLocation: Option[Location]

  CStructOrUnionDef* = ref object of ASTNode
    name: string
    body: ASTNode
    union: bool

  EnumDef* = ref object of ASTNode
    name: Path
    members: seq[ASTNode]
    baseType: Option[ASTNode]
    doc: Option[string]
    visibility: Visibility

  ExternalVar* = ref object of ASTNode
    name: string
    typeSpec: ASTNode
    realName: Option[string]

  Alias* = ref object of ASTNode
    name: Path
    value: ASTNode
    doc: Option[string]
    visibility: Visibility

  Metaclass* = ref object of ASTNode
    name: ASTNode

  Cast* = ref object of ASTNode
    obj, to: ASTNode

  NilableCast* = ref object of ASTNode
    obj, to: ASTNode

  TypeOf* = ref object of ASTNode
    expressions: seq[ASTNode]

  Annotation* = ref object of ASTNode
    path: Path
    args: seq[ASTNode]
    namedArgs: Option[seq[NamedArgument]]
    doc: Option[string]

  MacroExpression* = ref object of ASTNode
    exp: ASTNode
    output: bool

  MacroLiteral* = ref object of ASTNode
    value: string

  MacroVerbatim* = ref object of UnaryExpression

  MacroIf* = ref object of ASTNode
    cond, then, `else`: ASTNode

  MacroFor* = ref object of ASTNode
    vars: seq[Var]
    exp, body: ASTNode

  MacroVar* = ref object of ASTNode
    name: string
    exps: Option[seq[ASTNode]]

  Underscore* = ref object of ASTNode

  Splat* = ref object of UnaryExpression

  DoubleSplat* = ref object of UnaryExpression

  MagicConstant* = ref object of ASTNode
    # name: TokenKind

  Asm* = ref object of ASTNode
    text: string
    outputs: Option[seq[AsmOperand]]
    inputs: Option[seq[AsmOperand]]
    clobbers: Option[seq[string]]
    volatile, alignstack, intel, canThrow: bool

  AsmOperand* = ref object of ASTNode
    constraint: string
    exp: ASTNode

  Visibility* = enum
    vPublic
    vProtected
    vPrivate

# ASTNode

method location*(self: ASTNode): Option[Location] {.base.} =
  self.privateLocation

method `location=`*(self: ASTNode, location: Option[Location]) {.base.} =
  self.privateLocation = location

method endLocation*(self: ASTNode): Option[Location] {.base.} =
  self.privateEndLocation

method `endLocation=`*(self: ASTNode, endLocation: Option[Location]) {.base.} =
  self.privateEndLocation = endLocation

proc at*[T](self: T, location: Option[Location]): T =
  self.location = location
  self

proc at*[T](self: T, location: Location): T =
  self.location = location.some
  self

proc at*[T](self: T, node: ASTNode): T =
  self.location = node.location
  self.endLocation = node.endLocation
  self

proc atEnd*[T](self: T, endLocation: Option[Location]): T =
  self.endLocation = endLocation
  self

proc atEnd*[T](self: T, endLocation: Location): T =
  self.endLocation = endLocation.some
  self

proc atEnd*[T](self: T, node: ASTNode): T =
  self.endLocation = node.endLocation
  self

method doc*(self: ASTNode): Option[string] {.base.} =
  string.none

method `doc=`*(self: ASTNode, doc: Option[string]) {.base.} =
  discard

method nameLocation*(self: ASTNode): Option[Location] {.base.} =
  Location.none

method nameSize*(self: ASTNode): int {.base.} =
  0

method `visibility=`*(self: ASTNode, visibility: Visibility) {.base.} =
  discard

method visibility*(self: ASTNode): Visibility {.base.} =
  vPublic

proc isNop*(self: ASTNode): bool =
  self of Nop

proc isTrueLiteral*(self: ASTNode): bool =
  self of BoolLiteral and cast[BoolLiteral](self).value

proc isFalseLiteral*(self: ASTNode): bool =
  self of BoolLiteral and not cast[BoolLiteral](self).value

method isSingleExpression*(self: ASTNode): Option[ASTNode] {.base.} =
  ASTNode.none

proc singleExpression*(self: ASTNode): ASTNode =
  self.isSingleExpression.get(self)

# Nop

proc newNop*: Nop = Nop()

# Expressions

proc newExpressions*(expressions: seq[ASTNode] = @[]): Expressions =
  Expressions(expressions: expressions, keyword: ekNone)

proc toExpressions*(obj: type(nil)): ASTNode =
  newNop()

proc toExpressions*[T](obj: Option[T]): ASTNode =
  if obj.isSome:
    obj.get.toExpressions
  else:
    newNop()

proc toExpressions*(obj: seq[ASTNode]): ASTNode =
  case obj.len:
    of 0:
      newNop()
    of 1:
      obj[0]
    else:
      newExpressions(obj)

proc toExpressions*(obj: ASTNode): ASTNode =
  obj

proc isEmpty*(self: Expressions): bool =
  self.expressions.len == 0

proc `[]`*(self: Expressions, i: int): ASTNode =
  self.expressions[i]

proc last*(self: Expressions): ASTNode =
  self.expressions[self.expressions.len - 1]

method location*(self: Expressions): Option[Location] =
  if self.privateLocation.isSome or self.isEmpty:
    self.privateLocation
  else:
    self.expressions[0].location

method endLocation*(self: Expressions): Option[Location] =
  if self.privateEndLocation.isSome or self.isEmpty:
    self.privateEndLocation
  else:
    self.last.endLocation

method isSingleExpression*(self: Expressions): Option[ASTNode] =
  if self.expressions.len == 1:
    self.expressions[0].singleExpression.some
  else:
    ASTNode.none

# NilLiteral

proc newNilLiteral*: NilLiteral = NilLiteral()

# BoolLiteral

proc newBoolLiteral*(value: bool): BoolLiteral =
  BoolLiteral(value: value)

# NumberKind

proc isFloat*(self: NumberKind): bool =
  self == nkF32 or self == nkF64

# NumberLiteral

# proc newNumberLiteral*: NumberLiteral = NumberLiteral()

# CharLiteral

# proc newCharLiteral*: CharLiteral = CharLiteral()

# StringLiteral

# proc newStringLiteral*: StringLiteral = StringLiteral()

# StringInterpolation

# proc newStringInterpolation*: StringInterpolation = StringInterpolation()

# SymbolLiteral

# proc newSymbolLiteral*: SymbolLiteral = SymbolLiteral()

# ArrayLiteral

proc newArrayLiteral(
  elements: seq[ASTNode] = @[],
  `of`, name = ASTNode.none,
): ArrayLiteral = ArrayLiteral(
  elements: elements,
  `of`: `of`,
  name: name,
)

# HashLiteral

# proc newHashLiteral*: HashLiteral = HashLiteral()

# HashEntry

# proc initHashEntry

# NamedTupleLiteral

# proc newNamedTupleLiteral*: NamedTupleLiteral = NamedTupleLiteral()

# NamedTupleEntry

# proc initNamedTupleEntry

# RangeLiteral

# proc newRangeLiteral*: RangeLiteral = RangeLiteral()

# RegexLiteral

# proc newRegexLiteral*: RegexLiteral = RegexLiteral()

# TupleLiteral

# proc newTupleLiteral*: TupleLiteral = TupleLiteral()

# Var

# proc newVar*: Var = Var()

# Block

# proc newBlock*: Block = Block()

# Call

proc newCall*(
  obj: Option[ASTNode],
  name: string,
  args: seq[ASTNode] = @[],
  `block` = Block.none,
  blockArg = ASTNode.none,
  namedArgs = seq[NamedArgument].none,
  global = false,
): Call = Call(
  obj: obj,
  name: name,
  args: args,
  `block`: `block`,
  blockArg: blockArg,
  namedArgs: namedArgs,
  global: global,

  nameSize: -1,
  visibility: vPublic,
  expansion: false,
  hasParentheses: false,
)

proc newCall*(
  obj: Option[ASTNode],
  name: string,
  args: varargs[ASTNode],
  global = false,
): Call = newCall(obj, name, @args, global = global)

proc globalCall*(
  name: string,
  args: varargs[ASTNode],
): Call = newCall(ASTNode.none, name, @args, global = true)

# NamedArgument

# proc newNamedArgument*: NamedArgument = NamedArgument()

# If

# proc newIf*: If = If()

# Unless

# proc newUnless*: Unless = Unless()

# Assign

# proc newAssign*: Assign = Assign()

# OpAssign

# proc newOpAssign*: OpAssign = OpAssign()

# MultiAssign

# proc newMultiAssign*: MultiAssign = MultiAssign()

# InstanceVar

# proc newInstanceVar*: InstanceVar = InstanceVar()

# ReadInstanceVar

# proc newReadInstanceVar*: ReadInstanceVar = ReadInstanceVar()

# ClassVar

# proc newClassVar*: ClassVar = ClassVar()

# Global

# proc newGlobal*: Global = Global()

# And

proc newAnd*(left, right: ASTNode): And =
  And(left: left, right: right)

# Or

proc newOr*(left, right: ASTNode): Or =
  Or(left: left, right: right)

# Arg

# proc newArg*: Arg = Arg()

# ProcNotation

# proc newProcNotation*: ProcNotation = ProcNotation()

# Def

proc newDef*(
  name: string,
  args: seq[Arg] = @[],
  body = nil.toExpressions,
  receiver = ASTNode.none,
  blockArg = Arg.none,
  returnType = ASTNode.none,
  macroDef = false,
  yields = int.none,
  abstract = false,
  splatIndex = int.none,
  doubleSplat = Arg.none,
  freeVars = seq[string].none,
): Def = Def(
  name: name,
  args: args,
  body: body.toExpressions,
  receiver: receiver,
  blockArg: blockArg,
  returnType: returnType,
  macroDef: macroDef,
  yields: yields,
  abstract: abstract,
  splatIndex: splatIndex,
  doubleSplat: doubleSplat,
  freeVars: freeVars,

  visibility: vPublic,
  callsSuper: false,
  callsInitialize: false,
  callsPreviousDef: false,
  usesBlockArg: false,
  assignsSpecialVar: false,
)

# Macro

proc newMacro*(
  name: string,
  args: seq[Arg] = @[],
  body = newNop(),
  blockArg = Arg.none,
  splatIndex = int.none,
  doubleSplat = Arg.none,
): Macro = Macro(
  name: name,
  args: args,
  body: body,
  blockArg: blockArg,
  splatIndex: splatIndex,
  doubleSplat: doubleSplat,

  visibility: vPublic,
)

# Not

proc newNot*(exp: ASTNode): Not =
  Not(exp: exp)

# PointerOf

proc newPointerOf*(exp: ASTNode): PointerOf =
  PointerOf(exp: exp)

# SizeOf

proc newSizeOf*(exp: ASTNode): SizeOf =
  SizeOf(exp: exp)

# InstanceSizeOf

proc newInstanceSizeOf*(exp: ASTNode): InstanceSizeOf =
  InstanceSizeOf(exp: exp)

# Out

proc newOut*(exp: ASTNode): Out =
  Out(exp: exp)

# OffsetOf

# proc newOffsetOf*: OffsetOf = OffsetOf()

# VisibilityModifier

# proc newVisibilityModifier*: VisibilityModifier = VisibilityModifier()

# IsA

# proc newIsA*: IsA = IsA()

# RespondsTo

# proc newRespondsTo*: RespondsTo = RespondsTo()

# Require

# proc newRequire*: Require = Require()

# When

# proc newWhen*: When = When()

# Case

# proc newCase*: Case = Case()

# SelectWhen

# proc initSelectWhen

# Select

# proc newSelect*: Select = Select()

# ImplicitObj

# proc newImplicitObj*: ImplicitObj = ImplicitObj()

# Path

# proc newPath*: Path = Path()

# ClassDef

# proc newClassDef*: ClassDef = ClassDef()

# ModuleDef

# proc newModuleDef*: ModuleDef = ModuleDef()

# AnnotationDef

# proc newAnnotationDef*: AnnotationDef = AnnotationDef()

# While

# proc newWhile*: While = While()

# Until

# proc newUntil*: Until = Until()

# Generic

# proc newGeneric*: Generic = Generic()

# TypeDeclaration

# proc newTypeDeclaration*: TypeDeclaration = TypeDeclaration()

# UninitializedVar

# proc newUninitializedVar*: UninitializedVar = UninitializedVar()

# Rescue

# proc newRescue*: Rescue = Rescue()

# ExceptionHandler

# proc newExceptionHandler*: ExceptionHandler = ExceptionHandler()

# ProcLiteral

# proc newProcLiteral*: ProcLiteral = ProcLiteral()

# ProcPointer

# proc newProcPointer*: ProcPointer = ProcPointer()

# Union

# proc newUnion*: Union = Union()

# Self

# proc newSelf*: Self = Self()

# Return

proc newReturn*(exp = ASTNode.none): Return =
  Return(exp: exp)

# Break

proc newBreak*(exp = ASTNode.none): Break =
  Break(exp: exp)

# Next

proc newNext*(exp = ASTNode.none): Next =
  Next(exp: exp)

# Yield

# proc newYield*: Yield = Yield()

# Include

# proc newInclude*: Include = Include()

# Extend

# proc newExtend*: Extend = Extend()

# LibDef

# proc newLibDef*: LibDef = LibDef()

# FunDef

# proc newFunDef*: FunDef = FunDef()

# TypeDef

# proc newTypeDef*: TypeDef = TypeDef()

# CStructOrUnionDef

# proc newCStructOrUnionDef*: CStructOrUnionDef = CStructOrUnionDef()

# EnumDef

# proc newEnumDef*: EnumDef = EnumDef()

# ExternalVar

# proc newExternalVar*: ExternalVar = ExternalVar()

# Alias

# proc newAlias*: Alias = Alias()

# Metaclass

# proc newMetaclass*: Metaclass = Metaclass()

# Cast

# proc newCast*: Cast = Cast()

# NilableCast

# proc newNilableCast*: NilableCast = NilableCast()

# TypeOf

# proc newTypeOf*: TypeOf = TypeOf()

# Annotation

# proc newAnnotation*: Annotation = Annotation()

# MacroExpression

# proc newMacroExpression*: MacroExpression = MacroExpression()

# MacroLiteral

# proc newMacroLiteral*: MacroLiteral = MacroLiteral()

# MacroVerbatim

proc newMacroVerbatim*(exp: ASTNode): MacroVerbatim =
  MacroVerbatim(exp: exp)

# MacroIf

# proc newMacroIf*: MacroIf = MacroIf()

# MacroFor

# proc newMacroFor*: MacroFor = MacroFor()

# MacroVar

# proc newMacroVar*: MacroVar = MacroVar()

# Underscore

# proc newUnderscore*: Underscore = Underscore()

# Splat

proc newSplat*(exp: ASTNode): Splat =
  Splat(exp: exp)

# DoubleSplat

proc newDoubleSplat*(exp: ASTNode): DoubleSplat =
  DoubleSplat(exp: exp)

# MagicConstant

# proc newMagicConstant*(name: TokenKind): MagicConstant =
#   MagicConstant(name: name)

# Asm

proc newAsm*(
  text: string,
  outputs = seq[AsmOperand].none,
  inputs = seq[AsmOperand].none,
  clobbers = seq[string].none,
  volatile = false,
  alignstack = false,
  intel = false,
  canThrow = false,
): Asm = Asm(
  text: text,
  outputs: outputs,
  inputs: inputs,
  clobbers: clobbers,
  volatile: volatile,
  alignstack: alignstack,
  intel: intel,
  canThrow: canThrow,
)

# AsmOperand

proc newAsmOperand*(
  constraint: string,
  exp: ASTNode,
): AsmOperand = AsmOperand(
  constraint: constraint,
  exp: exp,
)

if isMainModule:
  assert ASTNode().location.isNone
  let expressions = newExpressions().at(newLocation("foo", 12, 34))
  expressions.keyword = ekParen
  assert expressions.keyword == ekParen
  assert expressions.expressions.len == 0
  assert expressions.location.get.filename == "foo"
  let arrayLiteral = newArrayLiteral()
  assert arrayLiteral.of.isNone
  let call = newCall(some[ASTNode](arrayLiteral), "==", arrayLiteral)
  assert call.obj.get of ArrayLiteral
  assert nil.toExpressions of Nop

  let foo1 = newNop().at(newLocation("a", 2, 3));
  let foo2 = newExpressions(@[cast[ASTNode](foo1)])
  assert foo2.location.get.filename == "a"
  discard foo2.at(newLocation("b", 5, 6))
  assert foo2.location.get.filename == "b"
