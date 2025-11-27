{-# LANGUAGE DeriveDataTypeable #-}

module Move.AST where

import Data.Data (Data, Typeable)

-- Below comments are extracted from the Move grammar as found on
-- https://github.dev/move-language/move/blob/main/language/move-compiler/src/parser/syntax.reverse
--
-- It is not complete but gives an high-level view of how an Expression is parsed by the Move compiler
-- Rules marked as "spec only" refer to Move specification language and for now are not considered by this parser

--      Exp =
--            <LambdaBindList> <Exp>        spec only
--          | <Quantifier>                  spec only
--          | <BinOpExp>
--          | <UnaryExp> "=" <Exp>

--      BinOpExp =
--          <BinOpExp> <BinOp> <BinOpExp>
--          | <UnaryExp>
--      BinOp = (listed from lowest to highest precedence)
--          "==>"                                       spec only
--          | "||"
--          | "&&"
--          | "==" | "!=" | '<' | ">" | "<=" | ">="
--          | ".."                                      spec only
--          | "|"
--          | "^"
--          | "&"
--          | "<<" | ">>"
--          | "+" | "-"
--          | "*" | "/" | "%"

--      UnaryExp =
--          "!" <UnaryExp>
--          | "&mut" <UnaryExp>
--          | "&" <UnaryExp>
--          | "*" <UnaryExp>
--          | "move" <Var>
--          | "copy" <Var>
--          | <DotOrIndexChain>

--      DotOrIndexChain =
--          <DotOrIndexChain> "." <Identifier>
--          | <DotOrIndexChain> "[" <Exp> "]"                      spec only
--          | <Term>

--      Term =
--          "break"
--          | "continue"
--          | "vector" ('<' Comma<Type> ">")? "[" Comma<Exp> "]"
--          | <NameExp>               -- FIXME: TODO: Inferred from the Move compiler: represents a variable, a function call, a literal struct
--          | <Value>
--          | "(" Comma<Exp> ")"
--          | "(" <Exp> ":" <Type> ")"
--          | "(" <Exp> "as" <Type> ")"
--          | "{" <Sequence>
--          | "if" "(" <Exp> ")" <Exp> "else" "{" <Exp> "}"     -- FIXME: can be removed
--          | "if" "(" <Exp> ")" "{" <Exp> "}"                  -- FIXME: can be removed
--          | "if" "(" <Exp> ")" <Exp> ("else" <Exp>)?
--          | "while" "(" <Exp> ")" "{" <Exp> "}"         -- FIXME: can be removed
--          | "while" "(" <Exp> ")" <Exp> (SpecBlock)?    -- FIXME: ignoring SpecBlock
--          | "loop" <Exp>
--          | "loop" "{" <Exp> "}"      -- FIXME: can be removed
--          | "return" "{" <Exp> "}"    -- FIXME: simplified with Sequence
--          | "return" <Exp>?
--          | "abort" "{" <Exp> "}"     -- FIXME: can be removed
--          | "abort" <Exp>

--      Value =
--          "@" <LeadingAccessName>
--          | "true"
--          | "false"
--          | <Number>
--          | <NumberTyped>
--          | <ByteString>

--      NameExp =
--          <NameAccessChain> <OptionalTypeArgs> "{" Comma<ExpField> "}"
--          | <NameAccessChain> <OptionalTypeArgs> "(" Comma<Exp> ")"
--          | <NameAccessChain> "!" "(" Comma<Exp> ")"
--          | <NameAccessChain> <OptionalTypeArgs>

--      ExpField = <Field> <":" <Exp>>?

--      NameAccessChain = <LeadingNameAccess> ( "::" <Identifier> ( "::" <Identifier> )? )?

--      Type =
--          <NameAccessChain> ('<' Comma<Type> ">")?
--          | "&" <Type>
--          | "&mut" <Type>
--          | "|" Comma<Type> "|" Type   (spec only)
--          | "(" Comma<Type> ")"

--      UseDecl =
--          "use" <ModuleIdent> <UseAlias> ";" |
--          "use" <ModuleIdent> :: <UseMember> ";" |
--          "use" <ModuleIdent> :: "{" Comma<UseMember> "}" ";"

--      Sequence = <UseDecl>* (<SequenceItem> ";")* <Exp>? "}"

--      SequenceItem =
--          <Exp>
--          | "let" <BindList> (":" <Type>)? ("=" <Exp>)?

--      BindList =
--          <Bind>
--          | "(" Comma<Bind> ")"

--      Bind =
--          <Var>
--          | <NameAccessChain> <OptionalTypeArgs> "{" Comma<BindField> "}" -- FIXME: Move compiler is missing binding to positional struct

--      BindField = <Field> <":" <Bind>>?

--      FunctionDecl =      -- FIXME: this definition is missing lot of tokens
--          "fun"
--          <FunctionDefName> "(" Comma<Parameter> ")"
--          (":" <Type>)?
--          ("acquires" <NameAccessChain> ("," <NameAccessChain>)*)?
--          ("{" <Sequence> "}" | ";")
--
--
--

-- | Root of the AST. Can either be a module or a script
data Root
  = RModule Module
  | RScript Script
  deriving (Eq, Show, Read, Data, Typeable)

-- | A module consists in an address, identifier and top level elements
data Module
  = Module
  { moduleAddress :: Address,
    moduleIdentifier :: Identifier,
    -- Top levels are ordered from top to bottom
    moduleTopLevels :: [TopLevel]
  }
  deriving (Eq, Show, Read, Data, Typeable)

-- | A script is similar to a Module, but without an identifier and with a subset of top levels allowed
newtype Script
  = Script
  { scriptTopLevels :: [TopLevel]
  }
  deriving (Eq, Show, Read, Data, Typeable)

-- | An identifier is a name of a variable or module
newtype Identifier
  = Identifier String
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | Represents any top level construct
data TopLevel
  = TopLevelUse Use
  | TopLevelFriend Friend
  | TopLevelNamedStruct NamedStruct
  | TopLevelPositionalStruct PositionalStruct
  | TopLevelFunction Function
  | TopLevelConstant Constant
  deriving (Eq, Show, Read, Data, Typeable)

-- | Using another module
data Use
  = Use
  { useAddress :: Address,
    useIdentifier :: Identifier,
    useAlias :: Maybe Identifier,
    useMembers :: [UseMember]
  }
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A use can include specific members, each with an optional alias
data UseMember
  = UseMember
  { useMemberIdentifier :: Identifier,
    useMemberUseAlias :: Maybe Identifier
  }
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | Friend with another module
newtype Friend = Friend NameAccessChain
  deriving (Eq, Show, Read, Data, Typeable)

-- | Definition of a struct type
--
-- Example: `struct MyStruct<...>{ field1: type1, field2: type2 }`
data NamedStruct
  = NamedStruct
  { namedStructIdentifier :: Identifier,
    namedStructTypeParameters :: [TypeParameter],
    namedStructAbilities :: [Ability],
    namedStructFields :: [NamedField]
  }
  deriving (Eq, Show, Read, Data, Typeable)

-- | Definition of a positional struct
--
-- Example: `struct MyPositionalStruct<...>(type1, type2);`
data PositionalStruct
  = PositionalStruct
  { positionalStructIdentifier :: Identifier,
    positionalStructTypeParameters :: [TypeParameter],
    positionalStructAbilities :: [Ability],
    positionalStructFields :: [PositionalField]
  }
  deriving (Eq, Show, Read, Data, Typeable)

-- | Abilities of a struct
data Ability
  = Copy
  | Drop
  | Key
  | Store
  deriving (Eq, Show, Read, Data, Typeable)

-- | Named fields are field definitions inside a "normal" (named) struct
--
-- Example: `struct MyStruct<...>{ field1: type1, field2: type2 }`
data NamedField
  = NamedField
  { fieldIdentifier :: Identifier,
    fieldType :: Type
  }
  deriving (Eq, Show, Read, Data, Typeable)

-- | Positional fields are field definitions inside a positional struct
--
-- Example: `struct MyPositionalStruct<...>(type1, type2);`
newtype PositionalField = PositionalField Type
  deriving (Eq, Show, Read, Data, Typeable)

-- | Represents any type: type constructow with arguments, reference types, tuple types
--
-- See grammar comment for Type
--
-- Also includes a special `TypeUnknown` to be used when the type can not be inferred
data Type
  = TypeConstructor NameAccessChain [Type]
  | TypeImmutableRef Type
  | TypeMutableRef Type
  | TypeTuple [Type]
  | TypeUnknown
  | IntermediateTypeScopes
  | TypeArrow [Type]
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A function declaration
--
-- Example: `public entry my_func<...>(a: A, b: B): u64 acquires B {...}`
data Function
  = Function
  { functionHasNativeModifier :: Bool,
    functionVisibilityModifier :: Maybe VisibilityModifier,
    functionHasEntryModifier :: Bool,
    functionName :: Identifier,
    functionTypeParameters :: [TypeParameter],
    functionParameters :: [Parameter],
    functionReturnType :: Maybe Type,
    functionAcquires :: [NameAccessChain],
    functionBody :: Maybe Sequence,
    functionUUID :: Maybe AnnotatedUUID
  }
  deriving (Eq, Show, Read, Data, Typeable)

-- | Function visibility modifier
data VisibilityModifier
  = VisibilityModifierPublic
  | VisibilityModifierPackage
  | VisibilityModifierFriend
  deriving (Eq, Show, Read, Data, Typeable)

-- | A Type parameter in any declaration (struct, function)
--
-- Example: `struct MyStruct<TypeParam1 : copy + drop, phantom TypeParam2>{...}`
data TypeParameter
  = TypeParameter
  { typeParameterIsPhantom :: Bool,
    typeIdentifier :: Identifier, -- Not using Type since here we have just an identifier
    typeConstraints :: [Ability]
  }
  deriving (Eq, Show, Read, Data, Typeable)

-- | A function parameter, consisting in an identifier and its type
data Parameter
  = Parameter
  { parameterIdentifier :: Identifier,
    parameterType :: Type,
    parameterUUID :: Maybe AnnotatedUUID
  }
  deriving (Eq, Show, Read, Data, Typeable)

-- | A top level constant
--
-- Example: `const MY_CONSTANT: u64 = 3;`
data Constant
  = Constant
  { constantIdentifier :: Identifier,
    constantType :: Type,
    constantExpression :: Expr
  }
  deriving (Eq, Show, Read, Data, Typeable)

-- | Represents any expression.
--
-- Assignments are considered expressions, but not bindings
--
-- Control flow constructs such as if-then-else and loops are considered expressions
--
-- Also other control flow keywords such as return, abort, break and continue are considered expressions
--
-- A chain of expressions enclosed by braces {...} are called a Sequence. See related type definition
data Expr
  = BinaryOpExprExpr BinaryOpExpr -- e1 <op> e2
  | AssignmentExpr Assignment -- e1 = e2
  | UnaryOpExpr UnaryExpr -- <op> e1
  | DotOrIndexChainExpr DotOrIndexChain -- e1.a.b
  | ValueLiteral ValueLiteral -- 42
  | CommaExpr [Expr] -- (e1, .., en)
  | TypedExprTerm TypedExpr -- (e1 : t1)
  | CastingTerm Casting -- (e1 as t1)
  | NamedStructExprExpr NamedStructExpr -- MyStruct{..}
  | PositionalStructExprOrFunctionCallExpr PositionalStructExprOrFunctionCall -- Foo(..)
  | FunctionBangCallExpr FunctionBangCall -- assert!(..)
  | NameAccessChainExpr NameAccessChain -- myVariable
  | SequenceExpr Sequence -- { e1; e2; let ..; e3 }
  | IfThenElseTerm IfThenElse
  | WhileTerm While
  | Loop Expr
  | Return (Maybe Expr)
  | Abort Expr
  | Break
  | Continue
  | IntermediateExprExpr IntermediateExpr
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | Binary operators involve two expressions
data BinaryOpExpr
  = Or Expr Expr
  | And Expr Expr
  | Eq Expr Expr
  | Neq Expr Expr
  | Lt Expr Expr
  | Gt Expr Expr
  | Leq Expr Expr
  | Geq Expr Expr
  | BitwiseOr Expr Expr
  | BitwiseXor Expr Expr
  | BitwiseAnd Expr Expr
  | ShiftLeft Expr Expr
  | ShiftRight Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | An assignment is considered an expression. It is composed by `left_part = right_part`, both expressions
data Assignment
  = Assignment
  { assignmentLeft :: Expr,
    assignmentRight :: Expr
  }
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A unary expression involves a single expression or identifier
--
-- References and dereferences are considered unary expressions
data UnaryExpr
  = Negation Expr
  | MutableReference Expr
  | ImmutableReference Expr
  | Dereference Expr
  | MoveExpr Identifier
  | CopyExpr Identifier
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A dot chain is a serie of accesses via dot notation
--
-- Example `my_obj.inner.a`
--
-- Note: The data type name includes index chains, but they are present only in the Move specification language
data DotOrIndexChain
  = DotAccess
  { dotAccessLeft :: Expr,
    dotAccessRight :: Identifier
  }
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A while construct
data While
  = While
  { whileCondition :: Expr,
    whileExpr :: Expr
  }
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | If-then-else construct, with else branch optional
data IfThenElse
  = IfThenElse
  { ifThenElseCondition :: Expr,
    ifThenElseIfBranch :: Expr,
    ifThenElseElseBranch :: Maybe Expr
  }
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A casting is composed by an expression and the target type
data Casting
  = Casting
  { castingExpr :: Expr,
    castingType :: Type
  }
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A typed expression TODO: unsure when it is used
data TypedExpr
  = TypedExpr
  { typedExpr :: Expr,
    typedExprType :: Type
  }
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A literal value can either be an address, a boolean or a numeric value
data ValueLiteral
  = Address Address
  | Boolean Bool
  | Numerical Numerical
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | An address can either be named (string name) or numerical
data Address
  = NamedAddress Identifier
  | NumericalAddress Numerical
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A integer value
data Numerical
  = LiteralIntDec Int
  | LiteralIntHex String
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A literal named struct, used as expression
--
-- Example: `MyStruct<...>{a: 12, b: 24}`
data NamedStructExpr
  = NamedStructExpr
  { nseNameAccessChain :: NameAccessChain,
    nseTypeArgs :: [Type],
    nseFields :: [NamedStructExprField]
  }
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A field of a named struct is composed by an identifier (the label of the struct), and the related expression.
--
-- If the expression is the same identifier of the label, it can be omitted as a suger syntax
--
-- Example: `...{a: 12 + 3, b}`
data NamedStructExprField
  = NamedStructExprField
  { nsefIdentifier :: Identifier,
    nsefExpr :: Maybe Expr
  }
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A name access chain is an access to a variable, struct or function that might be declared on the current module or another one
--
-- Examples: `my_local_variable`, `friendModule::my_struct`, `moduleAddress::moduleIdentifier::my_function`
data NameAccessChain
  = LocalNameAccessChain Identifier
  | AliasedNameAccessChain Address Identifier
  | UnaliasedNameAccessChain Address Identifier Identifier
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | The following can either be a function call or a positional struct used as an expression.
--
-- It's not possible to distinguish a positional struct wrt a function call just by parsing.
--
-- Example: `let a = GuessWhoAmI(42, "unknown");`
data PositionalStructExprOrFunctionCall
  = PositionalStructExprOrFunctionCall
  { pseofcNameAccessChain :: NameAccessChain,
    pseofcTypeArgs :: [Type],
    pseofcFields :: [Expr]
  }
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A function call with a bang! before the left parenthesis. Used by `assert!()`
data FunctionBangCall
  = FunctionBangCall
  { fbcNameAccessChain :: NameAccessChain,
    fbcFields :: [Expr]
  }
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A Sequence starts and ends with braces {...} and it's a serie of use declarations separated by ';',
-- followed by a serie of expressions or bindings separated by ';',
-- and an optional final expression without ';'
--
-- Example: `{ use ...; use ...; let a = 12; a = 23; a + 1 }`
data Sequence
  = Sequence
  { sequenceUses :: [Use],
    sequenceItems :: [SequenceItem],
    sequenceEndExpr :: Maybe Expr
  }
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A sequence item can either be an expression or a binding. Always ends with a ';'
--
-- Examples: `let a: u64 = 12;`, `a + 2;`
data SequenceItem
  = SequenceItemExpr Expr
  | SequenceItemBindExpr Bindings
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A let can bind a single variable or multiple ones via tuple notation
--
-- Example: `let (a, b): (u64, bool) = (12, true);`
data Bindings
  = Bindings
  { bindings :: Binded,
    bindingsBindType :: Maybe Type,
    bindingsBindExpr :: Maybe Expr
  }
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A binding can either be a single bind, or multiple
--
-- Note that multiple bindings also include the case of a one-element tuple
--
-- Example of single bind: `let x = ...`
--
-- Example of multiple binds: `let (x) = (...)`, `let (a, b) = (...)`
data Binded = BindedSingle Bind | BindedTuple [Bind]
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A single binding (left side of the '=').
--
-- It can either bind an identifier, or perform pattern matching to bind labels of a struct.
--
-- Examples: `let a = 12`, `let MyStruct(a, b) = ...`
data Bind
  = BindIdentifier Identifier (Maybe AnnotatedUUID)
  | BindNamedStruct BindedNamedStruct
  | BindPositionalStruct BindedPositionalStruct
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A bind consisting in a pattern matching with a named struct
--
-- This binding can specify a partial pattern via '..'
--
-- Example: `let MyStruct<...>{a: my_a, b, ..} = ...`
data BindedNamedStruct
  = BindedNamedStruct
  { bnsNameAccessChain :: NameAccessChain,
    bnsTypeArgs :: [Type],
    bnsFields :: BindedFields
  }
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A bind consisting in a pattern matching with a positional struct
--
-- This binding can specify a partial pattern via '..'
--
-- Example: `let MyStruct<...>(a, b, ..) = ...`
data BindedPositionalStruct
  = BindedPositionalStruct
  { bpsNameAccessChain :: NameAccessChain,
    bpsTypeArgs :: [Type],
    bpsFields :: BindedFields
  }
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | Fields in a binding via pattern match, either named or positional struct
--
-- Additionally, a partial pattern can be used to skip fields
data BindedFields
  = BindedFields
  { hasPartialPattern :: Bool,
    bindedFields :: [BindedField]
  }
  deriving (Eq, Show, Read, Data, Typeable, Ord)

-- | A single field that is being binded via pattern matching
--
-- Optionally, it can have an inner bind to it
--
-- Examples: `let ...{...a: {...inner: inner_a}} = ...`, `let ...{b} = ...`
data BindedField
  = BindedField
  { bindFieldIdentifier :: Identifier,
    bindFieldInnerBind :: Maybe Bind,
    bindedFieldUUID :: Maybe AnnotatedUUID
  }
  deriving (Eq, Show, Read, Data, Typeable, Ord)

type AnnotatedUUID = Int

-- |
-- Nodes used as intermediate representations for some expressions
data IntermediateExpr
  =
    -- | Any `&[mut] a[.b.c]`
    IntermediateReferenceLocalState [Identifier] Type
    -- | Any `*a` on the right side
  | IntermediateGetDereferenceLocalState Expr Type
    -- | Any `a[.b.c]` where `a` is in the local state
  | IntermediateGetLocalState [Identifier] Type
    -- | Any `a[.b.c] = ...`
  | IntermediatePutLocalState [Identifier] Expr
    -- | Any `let a = ...` where `a` has to be inserted in the local state
  | IntermediatePostLocalState Identifier (Maybe Expr)
    -- | Represents pushing a new local scope on top of the existing ones
  | IntermediatePushScope Identifier
    -- | Represents popping the local scope from the existing ones
  | IntermediatePopScope Identifier
  deriving (Eq, Show, Read, Data, Typeable, Ord)
