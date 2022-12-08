from ast import NumberKind
from system as system import `$`
import location, options
import std/re, std/strutils

type
  Keyword* = enum
    kAbstract
    kAlias
    kAnnotation
    kAs
    kAsQuestion
    kAsm
    kBegin
    kBreak
    kCase
    kClass
    kDef
    kDo
    kElse
    kElsif
    kEnd
    kEnsure
    kEnum
    kExtend
    kFalse
    kFor
    kFun
    kIf
    kIn
    kInclude
    kInstanceSizeof
    kIsAQuestion
    kLib
    kMacro
    kModule
    kNext
    kNil
    kNilQuestion
    kOf
    kOffsetof
    kOut
    kPointerof
    kPrivate
    kProtected
    kRequire
    kRescue
    kRespondsToQuestion
    kReturn
    kSelect
    kSelf
    kSizeof
    kStruct
    kSuper
    kThen
    kTrue
    kType
    kTypeof
    kUninitialized
    kUnion
    kUnless
    kUntil
    kVerbatim
    kWhen
    kWhile
    kWith
    kYield

  TokenKind* = enum
    tEof
    tSpace
    tNewline

    tIdent
    tConst
    tInstanceVar
    tClassVar

    tChar
    tString
    tSymbol
    tNumber

    tUnderscore
    tComment

    tDelimiterStart
    tDelimiterEnd

    tStringArrayStart
    tInterpolationStart
    tSymbolArrayStart
    tStringArrayEnd

    tGlobal
    tGlobalMatchDataIndex

    tMagicDir
    tMagicEndLine
    tMagicFile
    tMagicLine

    tMacroLiteral
    tMacroExpressionStart
    tMacroControlStart
    tMacroVar
    tMacroEnd

    tOpBang                     # !
    tOpBangEq                  # !=
    tOpBangTilde               # !~
    tOpDollarQuestion          # $?
    tOpDollarTilde             # $~
    tOpPercent                  # %
    tOpPercentEq               # %=
    tOpPercentRcurly           # %}
    tOpAmp                      # &
    tOpAmpAmp                  # &&
    tOpAmpAmpEq               # &&=
    tOpAmpStar                 # &*
    tOpAmpStarStar            # &**
    tOpAmpStarEq              # &*=
    tOpAmpPlus                 # &+
    tOpAmpPlusEq              # &+=
    tOpAmpMinus                # &-
    tOpAmpMinusEq             # &-=
    tOpAmpEq                   # &=
    tOpLparen                   # (
    tOpRparen                   # )
    tOpStar                     # *
    tOpStarStar                # **
    tOpStarStarEq             # **=
    tOpStarEq                  # *=
    tOpPlus                     # +
    tOpPlusEq                  # +=
    tOpComma                    # ,
    tOpMinus                    # -
    tOpMinusEq                 # -=
    tOpMinusGt                 # ->
    tOpPeriod                   # .
    tOpPeriodPeriod            # ..
    tOpPeriodPeriodPeriod     # ...
    tOpSlash                    # /
    tOpSlashSlash              # //
    tOpSlashSlashEq           # //=
    tOpSlashEq                 # /=
    tOpColon                    # :
    tOpColonColon              # ::
    tOpSemicolon                # ;
    tOpLt                       # <
    tOpLtLt                    # <<
    tOpLtLtEq                 # <<=
    tOpLtEq                    # <=
    tOpLtEqGt                 # <=>
    tOpEq                       # =
    tOpEqEq                    # ==
    tOpEqEqEq                 # ===
    tOpEqGt                    # =>
    tOpEqTilde                 # =~
    tOpGt                       # >
    tOpGtEq                    # >=
    tOpGtGt                    # >>
    tOpGtGtEq                 # >>=
    tOpQuestion                 # ?
    tOpAtLsquare               # @[
    tOpLsquare                  # [
    tOpLsquareRsquare          # []
    tOpLsquareRsquareEq       # []=
    tOpLsquareRsquareQuestion # []?
    tOpRsquare                  # ]
    tOpCaret                    # ^
    tOpCaretEq                 # ^=
    tOpGrave                    # `
    tOpLcurly                   # {
    tOpLcurlyPercent           # {%
    tOpLcurlyLcurly            # {{
    tOpBar                      # |
    tOpBarEq                   # |=
    tOpBarBar                  # ||
    tOpBarBarEq               # ||=
    tOpRcurly                   # }
    tOpTilde                    # ~

  TokenValueKind* = enum
    tvChar
    tvString
    tvKeyword
    tvNone

  TokenValue* = object
    case kind*: TokenValueKind
    of tvChar: char*: char
    of tvString: string*: string
    of tvKeyword: keyword*: Keyword
    of tvNone: discard

  Token* = ref object
    kind*: TokenKind
    value*: TokenValue
    numberKind*: NumberKind
    lineNumber*, columnNumber*: int
    filename*: string
    delimiterState*: DelimiterState
    macroState*: MacroState
    passedBackslashNewline*: bool
    docBuffer*: Option[string]
    raw*: string
    start*: int
    invalidEscape*: bool
    privateLocation: Option[Location]

  MacroState* = object
    whitespace: bool
    nest, controlNest: int
    delimiterState: Option[DelimiterState]
    beginningOfLine, yields, comment: bool
    heredocs: Option[seq[DelimiterState]]

  DelimiterKind* = enum
    dkString
    dkRegex
    dkStringArray
    dkSymbolArray
    dkCommand
    dkHeredoc

  DelimiterState* = object
    case kind: DelimiterKind
    of dkHeredoc: heredocId: string
    else: nestChar, endChar: char
    openCount, heredocIndent: int
    allowEscapes: bool

proc toUnderscore(self: string): string =
  self.replacef(re"([A-Z])", "_$1")

proc `$`*(self: Keyword): string =
  case self
  of kAsQuestion: "as?"
  of kIsAQuestion: "is_a?"
  of kNilQuestion: "nil?"
  of kRespondsToQuestion: "responds_to?"
  else: system.`$`(self).toUnderscore[2 .. ^1].toLowerAscii

proc `$`*(self: TokenKind): string =
  case self
  of tOpBang:                     "!"
  of tOpBangEq:                  "!="
  of tOpBangTilde:               "!~"
  of tOpDollarQuestion:          "$?"
  of tOpDollarTilde:             "$~"
  of tOpPercent:                  "%"
  of tOpPercentEq:               "%="
  of tOpPercentRcurly:           "%}"
  of tOpAmp:                      "&"
  of tOpAmpAmp:                  "&&"
  of tOpAmpAmpEq:               "&&="
  of tOpAmpStar:                 "&*"
  of tOpAmpStarStar:            "&**"
  of tOpAmpStarEq:              "&*="
  of tOpAmpPlus:                 "&+"
  of tOpAmpPlusEq:              "&+="
  of tOpAmpMinus:                "&-"
  of tOpAmpMinusEq:             "&-="
  of tOpAmpEq:                   "&="
  of tOpLparen:                   "("
  of tOpRparen:                   ")"
  of tOpStar:                     "*"
  of tOpStarStar:                "**"
  of tOpStarStarEq:             "**="
  of tOpStarEq:                  "*="
  of tOpPlus:                     "+"
  of tOpPlusEq:                  "+="
  of tOpComma:                    ","
  of tOpMinus:                    "-"
  of tOpMinusEq:                 "-="
  of tOpMinusGt:                 "->"
  of tOpPeriod:                   "."
  of tOpPeriodPeriod:            ".."
  of tOpPeriodPeriodPeriod:     "..."
  of tOpSlash:                    "/"
  of tOpSlashSlash:              "//"
  of tOpSlashSlashEq:           "//="
  of tOpSlashEq:                 "/="
  of tOpColon:                    ":"
  of tOpColonColon:              "::"
  of tOpSemicolon:                ";"
  of tOpLt:                       "<"
  of tOpLtLt:                    "<<"
  of tOpLtLtEq:                 "<<="
  of tOpLtEq:                    "<="
  of tOpLtEqGt:                 "<=>"
  of tOpEq:                       "="
  of tOpEqEq:                    "=="
  of tOpEqEqEq:                 "==="
  of tOpEqGt:                    "=>"
  of tOpEqTilde:                 "=~"
  of tOpGt:                       ">"
  of tOpGtEq:                    ">="
  of tOpGtGt:                    ">>"
  of tOpGtGtEq:                 ">>="
  of tOpQuestion:                 "?"
  of tOpAtLsquare:               "@["
  of tOpLsquare:                  "["
  of tOpLsquareRsquare:          "[]"
  of tOpLsquareRsquareEq:       "[]="
  of tOpLsquareRsquareQuestion: "[]?"
  of tOpRsquare:                  "]"
  of tOpCaret:                    "^"
  of tOpCaretEq:                 "^="
  of tOpGrave:                    "`"
  of tOpLcurly:                   "{"
  of tOpLcurlyPercent:           "{%"
  of tOpLcurlyLcurly:            "{{"
  of tOpBar:                      "|"
  of tOpBarEq:                   "|="
  of tOpBarBar:                  "||"
  of tOpBarBarEq:               "||="
  of tOpRcurly:                   "}"
  of tOpTilde:                    "~"

  of tMagicDir: "__DIR__"
  of tMagicEndLine: "__END_LINE__"
  of tMagicFile: "__FILE__"
  of tMagicLine: "__LINE__"

  else: system.`$`(self).toUnderscore[2 .. ^1].toUpperAscii

proc defaultMacroState*(): MacroState =
  MacroState(
    whitespace: true,
    nest: 0,
    controlNest: 0,
    delimiterState: DelimiterState.none,
    beginningOfLine: true,
    yields: false,
    comment: false,
    heredocs: seq[DelimiterState].none,
  )

proc defaultDelimiterState*(): DelimiterState =
  DelimiterState(
    kind: dkString,
    nestChar: '\0',
    endChar: '\0',
    openCount: 0,
    heredocIndent: 0,
    allowEscapes: true,
  )

proc newToken*(): Token =
  Token(
    kind: tEof,
    value: TokenValue(kind: tvNone),
    numberKind: nkI32,
    lineNumber: 0,
    columnNumber: 0,
    delimiterState: defaultDelimiterState(),
    macroState: defaultMacroState(),
    passedBackslashNewline: false,
    raw: "",
    start: 0,
    invalidEscape: false,
  )

proc location*(self: var Token): Location =
  if self.privateLocation.isNone:
    self.privateLocation = newLocation(
      self.filename,
      self.lineNumber,
      self.columnNumber,
    ).some
  result = self.privateLocation.get

proc `location=`*(self: var Token, location: Option[Location]) =
  self.privateLocation = location

proc `$`*(self: Token): string =
  case self.value.kind
  of tvChar: $self.value.char
  of tvString: self.value.string
  of tvKeyword: $self.value.keyword
  of tvNone: $self.kind

if isMainModule:
  assert $tMagicLine == "__LINE__"
  assert $tNumber == "NUMBER"
  assert $kInstanceSizeof == "instance_sizeof"
  assert $kNilQuestion == "nil?"
  let token = newToken()
  assert token.value.kind == tvNone