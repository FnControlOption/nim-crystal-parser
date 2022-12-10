import
  std/options,
  ./location

export options, location

type
  ASTNode* = ref object of RootObj
    private_location, private_endLocation: Option[Location]

  Nop* = ref object of ASTNode

  ExpressionsKeyword* = enum
    ekNone
    ekParen
    ekBegin

  Expressions* = ref object of ASTNode
    expressions*: seq[ASTNode]
    keyword*: ExpressionsKeyword

  NilLiteral* = ref object of ASTNode

  BoolLiteral* = ref object of ASTNode
    value*: bool

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
    value*: string
    kind*: NumberKind

  CharLiteral* = ref object of ASTNode
    value*: char

  StringLiteral* = ref object of ASTNode
    value*: string

  StringInterpolation* = ref object of ASTNode
    expressions*: seq[ASTNode]

  SymbolLiteral* = ref object of ASTNode
    value*: string

  ArrayLiteral* = ref object of ASTNode
    elements*: seq[ASTNode]
    `of`*, name*: Option[ASTNode]

  HashLiteral* = ref object of ASTNode
    entries*: seq[HashEntry]
    `of`*: Option[HashEntry]
    name*: Option[ASTNode]

  HashEntry* = object
    key*, value*: ASTNode

  NamedTupleLiteral* = ref object of ASTNode
    entries*: seq[NamedTupleEntry]

  NamedTupleEntry* = object
    key*: string
    value*: ASTNode

  RangeLiteral* = ref object of ASTNode
    `from`*, to*: ASTNode
    exclusive*: bool

  RegexLiteral* = ref object of ASTNode
    value*: ASTNode
    # options

  TupleLiteral* = ref object of ASTNode
    elements*: seq[ASTNode]

  # SpecialVar

  Var* = ref object of ASTNode
    name*: string

  Block* = ref object of ASTNode
    args*: seq[Var]
    body*: ASTNode
    call*: Option[Call]
    splatIndex*: Option[int]

  Call* = ref object of ASTNode
    obj*: Option[ASTNode]
    name*: string
    args*: seq[ASTNode]
    `block`*: Option[Block]
    blockArg*: Option[ASTNode]
    namedArgs*: Option[seq[NamedArgument]]
    nameLocation*: Option[Location]
    nameSize*: int
    doc*: Option[string]
    visibility*: Visibility
    global*, expansion*, hasParentheses*: bool

  NamedArgument* = ref object of ASTNode
    name*: string
    value*: ASTNode

  If* = ref object of ASTNode
    cond*, then*, `else`*: ASTNode
    ternary*: bool
    elseLocation*: Option[Location]

  Unless* = ref object of ASTNode
    cond*, then*, `else`*: ASTNode
    elseLocation*: Option[Location]

  Assign* = ref object of ASTNode
    target*, value*: ASTNode
    doc*: Option[string]

  OpAssign* = ref object of ASTNode
    target*: ASTNode
    op*: string
    value*: ASTNode
    nameLocation*: Option[Location]

  MultiAssign* = ref object of ASTNode
    targets*, values*: seq[ASTNode]

  InstanceVar* = ref object of ASTNode
    name*: string

  ReadInstanceVar* = ref object of ASTNode
    obj*: ASTNode
    name*: string

  ClassVar* = ref object of ASTNode
    name*: string

  Global* = ref object of ASTNode
    name*: string

  BinaryOp* = ref object of ASTNode
    left*, right*: ASTNode

  And* = ref object of BinaryOp

  Or* = ref object of BinaryOp

  Arg* = ref object of ASTNode
    name*, externalName*: string
    defaultValue*, restriction*: Option[ASTNode]
    doc*: Option[string]
    parsedAnnotations*: Option[seq[Annotation]]

  ProcNotation* = ref object of ASTNode
    inputs*: Option[seq[ASTNode]]
    output*: Option[ASTNode]

  Def* = ref object of ASTNode
    freeVars*: Option[seq[string]]
    receiver*: Option[ASTNode]
    name*: string
    args*: seq[Arg]
    doubleSplat*: Option[Arg]
    body*: ASTNode
    blockArg*: Option[Arg]
    returnType*: Option[ASTNode]
    yields*: Option[int]
    nameLocation*: Option[Location]
    splatIndex*: Option[int]
    doc*: Option[string]
    visibility*: Visibility

    macroDef*: bool
    callsSuper*: bool
    callsInitialize*: bool
    callsPreviousDef*: bool
    usesBlockArg*: bool
    assignsSpecialVar*: bool
    abstract*: bool

  Macro* = ref object of ASTNode
    name*: string
    args*: seq[Arg]
    body*: ASTNode
    doubleSplat*, blockArg*: Option[Arg]
    nameLocation*: Option[Location]
    splatIndex*: Option[int]
    doc*: Option[string]
    visibility*: Visibility

  UnaryExpression* = ref object of ASTNode
    exp*: ASTNode

  Not* = ref object of UnaryExpression

  PointerOf* = ref object of UnaryExpression

  SizeOf* = ref object of UnaryExpression

  InstanceSizeOf* = ref object of UnaryExpression

  Out* = ref object of UnaryExpression

  OffsetOf* = ref object of ASTNode
    offsetofType*, offset*: ASTNode

  VisibilityModifier* = ref object of ASTNode
    modifier*: Visibility
    exp*: ASTNode
    doc*: Option[string]

  IsA* = ref object of ASTNode
    obj*, `const`*: ASTNode
    nilCheck*: bool

  RespondsTo* = ref object of ASTNode
    obj*: ASTNode
    name*: string

  Require* = ref object of ASTNode
    string*: string

  When* = ref object of ASTNode
    conds*: seq[ASTNode]
    body*: ASTNode
    exhaustive*: bool

  Case* = ref object of ASTNode
    cond*: Option[ASTNode]
    whens*: seq[When]
    `else`*: Option[ASTNode]
    exhaustive*: bool

  SelectWhen* = object
    condition*, body*: ASTNode

  Select* = ref object of ASTNode
    whens*: seq[SelectWhen]
    `else`*: Option[ASTNode]

  ImplicitObj* = ref object of ASTNode

  Path* = ref object of ASTNode
    names*: seq[string]
    private_global: bool
    visibility*: Visibility

  ClassDef* = ref object of ASTNode
    name*: Path
    body*: ASTNode
    superclass*: Option[ASTNode]
    typeVars*: Option[seq[string]]
    nameLocation*: Option[Location]
    doc*: Option[string]
    splatIndex*: Option[int]
    abstract*: bool
    struct*: bool
    visibility*: Visibility

  ModuleDef* = ref object of ASTNode
    name*: Path
    body*: ASTNode
    typeVars*: Option[seq[string]]
    splatIndex*: Option[int]
    nameLocation*: Option[Location]
    doc*: Option[string]
    visibility*: Visibility

  AnnotationDef* = ref object of ASTNode
    name*: Path
    doc*: Option[string]
    nameLocation*: Option[Location]

  While* = ref object of ASTNode
    cond*, body*: ASTNode

  Until* = ref object of ASTNode
    cond*, body*: ASTNode

  Generic* = ref object of ASTNode
    name*: ASTNode
    typeVars*: seq[ASTNode]
    namedArgs*: Option[seq[NamedArgument]]
    suffix*: GenericSuffix

  GenericSuffix* = enum
    gsNone
    gsQuestion
    gsAsterisk
    gsBracket

  TypeDeclaration* = ref object of ASTNode
    `var`*, declaredType*: ASTNode
    value*: Option[ASTNode]

  UninitializedVar* = ref object of ASTNode
    `var`*, declaredType*: ASTNode

  Rescue* = ref object of ASTNode
    body*: ASTNode
    types*: Option[seq[ASTNode]]
    name*: Option[string]

  ExceptionHandler* = ref object of ASTNode
    body*: ASTNode
    rescues*: Option[seq[Rescue]]
    `else`*, ensure*: Option[ASTNode]
    implicit*, suffix*: bool
    elseLocation*, ensureLocation*: Option[Location]

  ProcLiteral* = ref object of ASTNode
    def*: Def

  ProcPointer* = ref object of ASTNode
    obj*: Option[ASTNode]
    name*: string
    args*: seq[ASTNode]
    global*: bool

  Union* = ref object of ASTNode
    types*: seq[ASTNode]

  Self* = ref object of ASTNode

  ControlExpression* = ref object of ASTNode
    exp*: Option[ASTNode]

  Return* = ref object of ControlExpression

  Break* = ref object of ControlExpression

  Next* = ref object of ControlExpression

  Yield* = ref object of ASTNode
    exps*: seq[ASTNode]
    scope*: Option[ASTNode]
    hasParentheses*: bool

  Include* = ref object of ASTNode
    name*: ASTNode

  Extend* = ref object of ASTNode
    name*: ASTNode

  LibDef* = ref object of ASTNode
    name*: string
    body*: ASTNode
    nameLocation*: Option[Location]
    visibility*: Visibility

  FunDef* = ref object of ASTNode
    name*: string
    args*: seq[Arg]
    returnType*, body*: Option[ASTNode]
    realName*: string
    doc*: Option[string]
    `varargs`*: bool

  TypeDef* = ref object of ASTNode
    name*: string
    typeSpec*: ASTNode
    nameLocation*: Option[Location]

  CStructOrUnionDef* = ref object of ASTNode
    name*: string
    body*: ASTNode
    union*: bool

  EnumDef* = ref object of ASTNode
    name*: Path
    members*: seq[ASTNode]
    baseType*: Option[ASTNode]
    doc*: Option[string]
    visibility*: Visibility

  ExternalVar* = ref object of ASTNode
    name*: string
    typeSpec*: ASTNode
    realName*: Option[string]

  Alias* = ref object of ASTNode
    name*: Path
    value*: ASTNode
    doc*: Option[string]
    visibility*: Visibility

  Metaclass* = ref object of ASTNode
    name*: ASTNode

  Cast* = ref object of ASTNode
    obj*, to*: ASTNode

  NilableCast* = ref object of ASTNode
    obj*, to*: ASTNode

  TypeOf* = ref object of ASTNode
    expressions*: seq[ASTNode]

  Annotation* = ref object of ASTNode
    path*: Path
    args*: seq[ASTNode]
    namedArgs*: Option[seq[NamedArgument]]
    doc*: Option[string]

  MacroExpression* = ref object of ASTNode
    exp*: ASTNode
    output*: bool

  MacroLiteral* = ref object of ASTNode
    value*: string

  MacroVerbatim* = ref object of UnaryExpression

  MacroIf* = ref object of ASTNode
    cond*, then*, `else`*: ASTNode

  MacroFor* = ref object of ASTNode
    vars*: seq[Var]
    exp*, body*: ASTNode

  MacroVar* = ref object of ASTNode
    name*: string
    exps*: Option[seq[ASTNode]]

  Underscore* = ref object of ASTNode

  Splat* = ref object of UnaryExpression

  DoubleSplat* = ref object of UnaryExpression

  MagicConstant* = ref object of ASTNode
    # name*: TokenKind

  Asm* = ref object of ASTNode
    text*: string
    outputs*: Option[seq[AsmOperand]]
    inputs*: Option[seq[AsmOperand]]
    clobbers*: Option[seq[string]]
    volatile*, alignstack*, intel*, canThrow*: bool

  AsmOperand* = ref object of ASTNode
    constraint*: string
    exp*: ASTNode

  Visibility* = enum
    vPublic
    vProtected
    vPrivate

# ASTNode

method location*(self: ASTNode): Option[Location] {.base.} =
  self.private_location

method `location=`*(self: ASTNode, location: Option[Location]) {.base.} =
  self.private_location = location

method `location=`*(self: ASTNode, location: Location) {.base.} =
  self.private_location = location.some

method endLocation*(self: ASTNode): Option[Location] {.base.} =
  self.private_endLocation

method `endLocation=`*(self: ASTNode, endLocation: Option[Location]) {.base.} =
  self.private_endLocation = endLocation

method `endLocation=`*(self: ASTNode, endLocation: Location) {.base.} =
  self.private_endLocation = endLocation.some

proc at*[T: ASTNode](self: T, location: Option[Location]): T =
  self.location = location
  self

proc at*[T: ASTNode](self: T, location: Location): T =
  self.location = location.some
  self

proc at*[T: ASTNode](self: T, node: ASTNode): T =
  self.location = node.location
  self.endLocation = node.endLocation
  self

proc atEnd*[T: ASTNode](self: T, endLocation: Option[Location]): T =
  self.endLocation = endLocation
  self

proc atEnd*[T: ASTNode](self: T, endLocation: Location): T =
  self.endLocation = endLocation.some
  self

proc atEnd*[T: ASTNode](self: T, node: ASTNode): T =
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
  self of BoolLiteral and self.BoolLiteral.value

proc isFalseLiteral*(self: ASTNode): bool =
  self of BoolLiteral and not self.BoolLiteral.value

method isSingleExpression*(self: ASTNode): Option[ASTNode] {.base.} =
  ASTNode.none

proc singleExpression*(self: ASTNode): ASTNode =
  self.isSingleExpression.get(self)

# Nop

proc new*(T: type Nop): Nop =
  Nop()

# Expressions

proc new*(T: type Expressions, expressions: seq[ASTNode] = @[]): Expressions =
  Expressions(expressions: expressions, keyword: ekNone)

proc `from`*(T: type Expressions, obj: type(nil)): ASTNode =
  Nop.new

proc `from`*[O](T: type Expressions, obj: Option[O]): ASTNode =
  if obj.isSome:
    Expressions.`from`(obj.get)
  else:
    Nop.new

proc `from`*(T: type Expressions, obj: seq[ASTNode]): ASTNode =
  case obj.len:
    of 0:
      Nop.new
    of 1:
      obj[0]
    else:
      Expressions.new(obj)

proc `from`*(T: type Expressions, obj: ASTNode): ASTNode =
  obj

proc isEmpty*(self: Expressions): bool =
  self.expressions.len == 0

proc `[]`*(self: Expressions, i: int): ASTNode =
  self.expressions[i]

proc last*(self: Expressions): ASTNode =
  self.expressions[self.expressions.len - 1]

method location*(self: Expressions): Option[Location] =
  if self.private_location.isSome or self.isEmpty:
    self.private_location
  else:
    self.expressions[0].location

method endLocation*(self: Expressions): Option[Location] =
  if self.private_endLocation.isSome or self.isEmpty:
    self.private_endLocation
  else:
    self.last.endLocation

method isSingleExpression*(self: Expressions): Option[ASTNode] =
  if self.expressions.len == 1:
    self.expressions[0].singleExpression.some
  else:
    ASTNode.none

# NilLiteral

proc new*(T: type NilLiteral): NilLiteral =
  NilLiteral()

# BoolLiteral

proc new*(T: type BoolLiteral, value: bool): BoolLiteral =
  BoolLiteral(value: value)

# NumberKind

proc isFloat*(self: NumberKind): bool =
  self == nkF32 or self == nkF64

# NumberLiteral

proc new*(
  T: type NumberLiteral,
  value: string,
  kind: NumberKind,
): NumberLiteral =
  NumberLiteral(value: value, kind: kind)

# CharLiteral

proc new*(T: type CharLiteral, value: char): CharLiteral =
  CharLiteral(value: value)

# StringLiteral

# proc new*(T: type StringLiteral): StringLiteral =
#   StringLiteral()

# StringInterpolation

# proc new*(T: type StringInterpolation): StringInterpolation =
#   StringInterpolation()

# SymbolLiteral

# proc new*(T: type SymbolLiteral): SymbolLiteral =
#   SymbolLiteral()

# ArrayLiteral

proc new*(
  T: type ArrayLiteral,
  elements: seq[ASTNode] = @[],
  `of`, name = ASTNode.none,
): ArrayLiteral =
  ArrayLiteral(
    elements: elements,
    `of`: `of`,
    name: name,
  )

# HashLiteral

# proc new*(T: type HashLiteral): HashLiteral =
#   HashLiteral()

# HashEntry

# proc init(T: type HashEntry)

# NamedTupleLiteral

# proc new*(T: type NamedTupleLiteral): NamedTupleLiteral =
#   NamedTupleLiteral()

# NamedTupleEntry

# proc init(T: type NamedTupleEntry)

# RangeLiteral

# proc new*(T: type RangeLiteral): RangeLiteral =
#   RangeLiteral()

# RegexLiteral

# proc new*(T: type RegexLiteral): RegexLiteral =
#   RegexLiteral()

# TupleLiteral

# proc new*(T: type TupleLiteral): TupleLiteral =
#   TupleLiteral()

# Var

proc new*(T: type Var, name: string): Var =
  Var(name: name)

# Block

# proc new*(T: type Block): Block =
#   Block()

# Call

proc new*(
  T: type Call,
  obj: Option[ASTNode],
  name: string,
  args: seq[ASTNode],
  `block` = Block.none,
  blockArg = ASTNode.none,
  namedArgs = seq[NamedArgument].none,
  global = false,
): Call =
  Call(
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

proc new*(
  T: type Call,
  obj: Option[ASTNode],
  name: string,
  args: varargs[ASTNode],
  global = false,
): Call =
  Call.new(obj, name, @args, global = global)

proc global*(
  T: type Call,
  name: string,
  args: varargs[ASTNode],
): Call =
  Call.new(ASTNode.none, name, @args, global = true)

# NamedArgument

# proc new*(T: type NamedArgument): NamedArgument =
#   NamedArgument()

# If

# proc new*(T: type If): If =
#   If()

# Unless

# proc new*(T: type Unless): Unless =
#   Unless()

# Assign

# proc new*(T: type Assign): Assign =
#   Assign()

# OpAssign

# proc new*(T: type OpAssign): OpAssign =
#   OpAssign()

# MultiAssign

# proc new*(T: type MultiAssign): MultiAssign =
#   MultiAssign()

# InstanceVar

# proc new*(T: type InstanceVar): InstanceVar =
#   InstanceVar()

# ReadInstanceVar

# proc new*(T: type ReadInstanceVar): ReadInstanceVar =
#   ReadInstanceVar()

# ClassVar

# proc new*(T: type ClassVar): ClassVar =
#   ClassVar()

# Global

# proc new*(T: type Global): Global =
#   Global()

# BinaryOp

proc new*(T: type BinaryOp, left, right: ASTNode): T =
  T(left: left, right: right)

# Arg

# proc new*(T: type Arg): Arg =
#   Arg()

# ProcNotation

# proc new*(T: type ProcNotation): ProcNotation =
#   ProcNotation()

# Def

proc new*(
  T: type Def,
  name: string,
  args: seq[Arg] = @[],
  body = Expressions.`from`(nil),
  receiver = ASTNode.none,
  blockArg = Arg.none,
  returnType = ASTNode.none,
  macroDef = false,
  yields = int.none,
  abstract = false,
  splatIndex = int.none,
  doubleSplat = Arg.none,
  freeVars = seq[string].none,
): Def =
  Def(
    name: name,
    args: args,
    body: Expressions.`from`(body),
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

proc new*(
  T: type Macro,
  name: string,
  args: seq[Arg] = @[],
  body = Nop.new,
  blockArg = Arg.none,
  splatIndex = int.none,
  doubleSplat = Arg.none,
): Macro =
  Macro(
    name: name,
    args: args,
    body: body,
    blockArg: blockArg,
    splatIndex: splatIndex,
    doubleSplat: doubleSplat,

    visibility: vPublic,
  )

# UnaryExpression

proc new*(T: type UnaryExpression, exp: ASTNode): T =
  T(exp: exp)

# OffsetOf

# proc new*(T: type OffsetOf): OffsetOf =
#   OffsetOf()

# VisibilityModifier

# proc new*(T: type VisibilityModifier): VisibilityModifier =
#   VisibilityModifier()

# IsA

# proc new*(T: type IsA): IsA =
#   IsA()

# RespondsTo

# proc new*(T: type RespondsTo): RespondsTo =
#   RespondsTo()

# Require

# proc new*(T: type Require): Require =
#   Require()

# When

# proc new*(T: type When): When =
#   When()

# Case

# proc new*(T: type Case): Case =
#   Case()

# SelectWhen

# proc init(T: type SelectWhen)

# Select

# proc new*(T: type Select): Select =
#   Select()

# ImplicitObj

# proc new*(T: type ImplicitObj): ImplicitObj =
#   ImplicitObj()

# Path

proc global*(self: Path): bool =
  result = self.private_global

proc `global=`*(self: Path, global: bool) =
  self.private_global = global

proc new*(T: type Path, names: seq[string], global = false): Path =
  Path(names: names, private_global: global, visibility: vPublic)

proc new*(T: type Path, names: varargs[string], global = false): Path =
  Path.new(@names, global)

proc global*(T: type Path, names: seq[string]): Path =
  Path.new(names, true)

proc global*(T: type Path, names: varargs[string]): Path =
  Path.new(names, true)

# ClassDef

# proc new*(T: type ClassDef): ClassDef =
#   ClassDef()

# ModuleDef

# proc new*(T: type ModuleDef): ModuleDef =
#   ModuleDef()

# AnnotationDef

# proc new*(T: type AnnotationDef): AnnotationDef =
#   AnnotationDef()

# While

# proc new*(T: type While): While =
#   While()

# Until

# proc new*(T: type Until): Until =
#   Until()

# Generic

proc new*(
  T: type Generic,
  name: ASTNode,
  typeVars: seq[ASTNode],
  namedArgs = seq[NamedArgument].none,
  suffix = gsNone,
): Generic =
  Generic(
    name: name,
    typeVars: typeVars,
    namedArgs: namedArgs,
    suffix: suffix,
  )

proc new*(T: type Generic, name, typeVar: ASTNode): Generic =
  Generic.new(name, @[typeVar])

# TypeDeclaration

# proc new*(T: type TypeDeclaration): TypeDeclaration =
#   TypeDeclaration()

# UninitializedVar

# proc new*(T: type UninitializedVar): UninitializedVar =
#   UninitializedVar()

# Rescue

# proc new*(T: type Rescue): Rescue =
#   Rescue()

# ExceptionHandler

# proc new*(T: type ExceptionHandler): ExceptionHandler =
#   ExceptionHandler()

# ProcLiteral

# proc new*(T: type ProcLiteral): ProcLiteral =
#   ProcLiteral()

# ProcPointer

# proc new*(T: type ProcPointer): ProcPointer =
#   ProcPointer()

# Union

proc new*(T: type Union, types: seq[ASTNode]): Union =
  Union(types: types)

# Self

# proc new*(T: type Self): Self =
#   Self()

# ControlExpression

proc new*(T: type ControlExpression, exp = ASTNode.none): T =
  T(exp: exp)

# Yield

# proc new*(T: type Yield): Yield =
#   Yield()

# Include

# proc new*(T: type Include): Include =
#   Include()

# Extend

# proc new*(T: type Extend): Extend =
#   Extend()

# LibDef

# proc new*(T: type LibDef): LibDef =
#   LibDef()

# FunDef

# proc new*(T: type FunDef): FunDef =
#   FunDef()

# TypeDef

# proc new*(T: type TypeDef): TypeDef =
#   TypeDef()

# CStructOrUnionDef

# proc new*(T: type CStructOrUnionDef): CStructOrUnionDef =
#   CStructOrUnionDef()

# EnumDef

# proc new*(T: type EnumDef): EnumDef =
#   EnumDef()

# ExternalVar

# proc new*(T: type ExternalVar): ExternalVar =
#   ExternalVar()

# Alias

# proc new*(T: type Alias): Alias =
#   Alias()

# Metaclass

# proc new*(T: type Metaclass): Metaclass =
#   Metaclass()

# Cast

# proc new*(T: type Cast): Cast =
#   Cast()

# NilableCast

# proc new*(T: type NilableCast): NilableCast =
#   NilableCast()

# TypeOf

# proc new*(T: type TypeOf): TypeOf =
#   TypeOf()

# Annotation

# proc new*(T: type Annotation): Annotation =
#   Annotation()

# MacroExpression

# proc new*(T: type MacroExpression): MacroExpression =
#   MacroExpression()

# MacroLiteral

# proc new*(T: type MacroLiteral): MacroLiteral =
#   MacroLiteral()

# MacroIf

# proc new*(T: type MacroIf): MacroIf =
#   MacroIf()

# MacroFor

# proc new*(T: type MacroFor): MacroFor =
#   MacroFor()

# MacroVar

# proc new*(T: type MacroVar): MacroVar =
#   MacroVar()

# Underscore

# proc new*(T: type Underscore): Underscore =
#   Underscore()

# MagicConstant

# proc new*(T: type MagicConstant, name: TokenKind): MagicConstant =
#   MagicConstant(name: name)

# Asm

proc new*(
  T: type Asm,
  text: string,
  outputs = seq[AsmOperand].none,
  inputs = seq[AsmOperand].none,
  clobbers = seq[string].none,
  volatile = false,
  alignstack = false,
  intel = false,
  canThrow = false,
): Asm =
  Asm(
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

proc new*(
  T: type AsmOperand,
  constraint: string,
  exp: ASTNode,
): AsmOperand =
  AsmOperand(
    constraint: constraint,
    exp: exp,
  )

if isMainModule:
  assert ASTNode().location.isNone

  let expressions = Expressions.new.at(Location.new("foo", 12, 34))
  expressions.keyword = ekParen
  assert expressions.keyword == ekParen
  assert expressions.expressions.len == 0
  assert expressions.location.get.filename == "foo"

  let arrayLiteral = ArrayLiteral.new
  assert arrayLiteral.of.isNone

  let call = Call.new(arrayLiteral.ASTNode.some, "==", arrayLiteral)
  assert call.obj.get of ArrayLiteral

  assert Expressions.`from`(nil) of Nop
  assert Expressions.`from`(ASTNode.none) of Nop
  assert Expressions.`from`(seq[ASTNode].none) of Nop

  let foo1 = Nop.new.at(Location.new("a", 2, 3));
  let foo2 = Expressions.new(@[foo1.ASTNode])
  assert foo2.location.get.filename == "a"
  discard foo2.at(Location.new("b", 5, 6))
  assert foo2.location.get.filename == "b"

  assert And.new of And
  assert Not.new of Not
  assert Return.new of Return
