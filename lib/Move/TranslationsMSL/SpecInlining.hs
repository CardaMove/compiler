-- |
-- Inlining of spec schemas for the MSL pipeline (`mslTranspiler`).
--
-- Every `include` of a function spec block is replaced by the members of the included schema, with the schema
-- variables substituted by the arguments. The design is in doc/schema_inlining.md
module Move.TranslationsMSL.SpecInlining (inlineSpecSchemas) where

import Control.Monad (forM, zipWithM)
import Control.Monad.State (State, evalState, get, put)
import Data.Generics.Uniplate.Data (descend, transformBi, universeBi)
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Move.AST
import Move.TranslationsMSL.PreProcessingMSL (stripUseMembers)

-- | The address and identifier of a module
type ModuleKey = (Address, Identifier)

-- | What the inlining needs to know about a root to resolve the names written in it
data RootContext = RootContext
  { -- | Position of the root in the list of roots
    rcIndex :: Int,
    -- | The module key, Nothing for a script
    rcModule :: Maybe ModuleKey,
    -- | The top level uses of the root
    rcUses :: [Use],
    -- | The names declared by the top levels of the root (structs, functions, constants, spec schemas)
    rcTopLevelNames :: Set.Set Identifier
  }

-- | A name resolved in the context of a root
data ResolvedName
  = -- | Declared by the root itself
    ResolvedOwn Identifier
  | -- | Declared by the given module (possibly not part of the project)
    ResolvedQualified Address Identifier Identifier

-- | A spec schema declaration: the index of its root, its name, its type parameters and its members
data SchemaDecl = SchemaDecl Int Identifier [TypeParameter] [SpecBlockMember]

-- | State of the pass: the counter for fresh names and the identifiers already used in the project
type InlineState = (Int, Set.Set Identifier)

-- |
-- Third pass of the MSL pipeline, run after `stripMoveFunctions` and before `annotateBindingsWithUUID`.
--
-- Given all the roots of the project, replaces each `include` of each function spec block with the members of the
-- included schema: names are qualified, schema lets are renamed to fresh names, type parameters and schema variables
-- are substituted, and a premise `p ==> S` is copied into each condition. Then removes every spec schema block,
-- removes the use members importing a schema of a project module, and adds a module import for each module the
-- inlined names refer to. Only `S{..}`, `S` and `p ==> s` are supported as schema expressions
inlineSpecSchemas :: [Root] -> [Root]
inlineSpecSchemas roots =
  let contexts = zipWith buildRootContext [0 ..] roots
      initialState = (0, Set.fromList (universeBi roots :: [Identifier]))
      inlined = evalState (zipWithM (inlineRoot roots contexts) contexts roots) initialState
      userModules = [m | RModule m <- roots]
   in map (\(root, neededModules) -> addModuleImports neededModules $ removeSchemas userModules root) inlined

-- |
-- Builds the context of a root, used to resolve the names written in it
buildRootContext :: Int -> Root -> RootContext
buildRootContext index root =
  RootContext
    { rcIndex = index,
      rcModule = case root of
        RModule Module {moduleAddress, moduleIdentifier} -> Just (moduleAddress, moduleIdentifier)
        RScript _ -> Nothing,
      rcUses = [u | TopLevelUse u <- rootTopLevels root],
      rcTopLevelNames = Set.fromList $ mapMaybe topLevelName (rootTopLevels root)
    }
  where
    -- The name declared by a top level, if any
    topLevelName :: TopLevel -> Maybe Identifier
    topLevelName (TopLevelNamedStruct NamedStruct {namedStructIdentifier}) = Just namedStructIdentifier
    topLevelName (TopLevelPositionalStruct PositionalStruct {positionalStructIdentifier}) = Just positionalStructIdentifier
    topLevelName (TopLevelFunction Function {functionName}) = Just functionName
    topLevelName (TopLevelConstant Constant {constantIdentifier}) = Just constantIdentifier
    topLevelName (TopLevelSpecBlock SpecBlock {specBlockTarget = SpecBlockTargetSchema ident _}) = Just ident
    topLevelName _ = Nothing

-- |
-- Returns the top levels of a root
rootTopLevels :: Root -> [TopLevel]
rootTopLevels (RModule Module {moduleTopLevels}) = moduleTopLevels
rootTopLevels (RScript Script {scriptTopLevels}) = scriptTopLevels

-- |
-- Replaces the top levels of a root
withTopLevels :: Root -> [TopLevel] -> Root
withTopLevels (RModule m) topLevels = RModule m {moduleTopLevels = topLevels}
withTopLevels (RScript s) topLevels = RScript s {scriptTopLevels = topLevels}

-- |
-- Resolves a name written in the given root, using only its uses and its top level declarations.
-- Returns Nothing if the name is not declared nor imported (e.g. a variable, a primitive type or a built-in)
resolveName :: RootContext -> NameAccessChain -> Maybe ResolvedName
resolveName RootContext {rcUses, rcTopLevelNames} (LocalNameAccessChain ident)
  | ident `Set.member` rcTopLevelNames = Just $ ResolvedOwn ident
  | otherwise =
      case [ ResolvedQualified useAddress useIdentifier useMemberIdentifier
             | Use {useAddress, useIdentifier, useMembers} <- rcUses,
               UseMember {useMemberIdentifier, useMemberUseAlias} <- useMembers,
               useMemberIdentifier /= Identifier "Self",
               fromMaybe useMemberIdentifier useMemberUseAlias == ident
           ] of
        resolved : _ -> Just resolved
        [] -> Nothing
resolveName RootContext {rcModule, rcUses} (AliasedNameAccessChain qualifier ident)
  | qualifier == Identifier "Self", Just _ <- rcModule = Just $ ResolvedOwn ident
  | otherwise =
      case [ResolvedQualified useAddress useIdentifier ident | use@Use {useAddress, useIdentifier} <- rcUses, qualifier `elem` moduleAliases use] of
        resolved : _ -> Just resolved
        [] -> Nothing
  where
    -- The names a use makes the imported module reachable with
    moduleAliases :: Use -> [Identifier]
    moduleAliases Use {useIdentifier, useAlias, useMembers} =
      maybe [useIdentifier] pure useAlias ++ [alias | UseMember {useMemberIdentifier = Identifier "Self", useMemberUseAlias = Just alias} <- useMembers]
resolveName _ (UnaliasedNameAccessChain address moduleIdent ident) = Just $ ResolvedQualified address moduleIdent ident

-- |
-- Builds the name access chain referring, from a root whose module is `fromModule`, to a name resolved
-- in a root whose module is `declModule` (Nothing for scripts).
-- A name of `fromModule` itself, or of a script, stays local; any other name is fully qualified
qualifyFor :: Maybe ModuleKey -> Maybe ModuleKey -> ResolvedName -> NameAccessChain
qualifyFor fromModule declModule resolved = case resolved of
  ResolvedQualified address moduleIdent ident -> qualified address moduleIdent ident
  ResolvedOwn ident -> case declModule of
    Nothing -> LocalNameAccessChain ident
    Just (address, moduleIdent) -> qualified address moduleIdent ident
  where
    -- Qualifies a name of the given module, unless it is `fromModule`
    qualified :: Address -> Identifier -> Identifier -> NameAccessChain
    qualified address moduleIdent ident
      | Just (address, moduleIdent) == fromModule = LocalNameAccessChain ident
      | otherwise = UnaliasedNameAccessChain address moduleIdent ident

-- |
-- Finds the spec schema declaration a resolved name refers to, among the given roots
findSchema :: [Root] -> [RootContext] -> RootContext -> ResolvedName -> Maybe SchemaDecl
findSchema roots _ RootContext {rcIndex} (ResolvedOwn ident) = schemaInRoot rcIndex (roots !! rcIndex) ident
findSchema roots contexts _ (ResolvedQualified address moduleIdent ident) =
  case find (\RootContext {rcModule} -> rcModule == Just (address, moduleIdent)) contexts of
    Just RootContext {rcIndex} -> schemaInRoot rcIndex (roots !! rcIndex) ident
    Nothing -> Nothing

-- |
-- Finds the spec schema with the given name among the top levels of a root
schemaInRoot :: Int -> Root -> Identifier -> Maybe SchemaDecl
schemaInRoot index root ident =
  case [SchemaDecl index ident tParams members | TopLevelSpecBlock SpecBlock {specBlockTarget = SpecBlockTargetSchema name tParams, specBlockMembers = members} <- rootTopLevels root, name == ident] of
    decl : _ -> Just decl
    [] -> Nothing

-- |
-- Inlines the includes of every function spec block of a root.
-- Returns the new root and the modules referred to by qualified names in the inlined members
inlineRoot :: [Root] -> [RootContext] -> RootContext -> Root -> State InlineState (Root, Set.Set ModuleKey)
inlineRoot roots contexts context root = do
  results <- forM (rootTopLevels root) inlineTopLevel
  let neededModules = Set.unions (map snd results)
  return (withTopLevels root (map fst results), neededModules)
  where
    -- Inlines the includes of a function spec block, leaving any other top level untouched
    inlineTopLevel :: TopLevel -> State InlineState (TopLevel, Set.Set ModuleKey)
    inlineTopLevel (TopLevelSpecBlock specBlock@SpecBlock {specBlockTarget = SpecBlockTargetFunction _ _, specBlockMembers}) = do
      expanded <- forM specBlockMembers $ \member -> case member of
        SpecBlockMemberInclude expr -> do
          members <- expandSchemaExpr roots contexts context [] expr
          return (members, Set.fromList [(address, moduleIdent) | UnaliasedNameAccessChain address moduleIdent _ <- universeBi members])
        _ -> return ([member], Set.empty)
      return (TopLevelSpecBlock specBlock {specBlockMembers = concatMap fst expanded}, Set.unions (map snd expanded))
    inlineTopLevel topLevel = return (topLevel, Set.empty)

-- |
-- Expands a schema expression written in the given root, returning the members that replace the include.
-- The stack holds the schemas being expanded, to detect cycles
expandSchemaExpr :: [Root] -> [RootContext] -> RootContext -> [(Int, Identifier)] -> Expr -> State InlineState [SpecBlockMember]
expandSchemaExpr roots contexts context stack expr = case expr of
  BinaryOpExprExpr (Implies premise schemaExpr) -> map (addPremise premise) <$> expandSchemaExpr roots contexts context stack schemaExpr
  CommaExpr [schemaExpr] -> expandSchemaExpr roots contexts context stack schemaExpr
  NamedStructExprExpr NamedStructExpr {nseNameAccessChain, nseTypeArgs, nseFields} ->
    applySchema roots contexts context stack nseNameAccessChain nseTypeArgs nseFields
  NameAccessChainExpr nac -> applySchema roots contexts context stack nac [] []
  BinaryOpExprExpr (And _ _) -> error $ "Schema conjunction `S1 && S2` in an include is not supported yet: " ++ show expr
  IfThenElseTerm _ -> error $ "Schema expression `if (p) S1 else S2` in an include is not supported yet: " ++ show expr
  _ -> error $ "Not a schema expression in an include: " ++ show expr

-- |
-- Copies the premise of a conditional include `p ==> S` into a member of S:
-- `ensures e` becomes `ensures p ==> e` and `aborts_if c` becomes `aborts_if p && c`,
-- while `modifies` (always active in MSL), lets and schema variables are unchanged
addPremise :: Expr -> SpecBlockMember -> SpecBlockMember
addPremise premise (SpecBlockMemberCondition condition@SpecCondition {specConditionKind, specConditionExpr}) =
  case specConditionKind of
    SpecConditionEnsures -> SpecBlockMemberCondition condition {specConditionExpr = BinaryOpExprExpr $ Implies premise specConditionExpr}
    SpecConditionAbortsIf -> SpecBlockMemberCondition condition {specConditionExpr = BinaryOpExprExpr $ And premise specConditionExpr}
    SpecConditionModifies -> SpecBlockMemberCondition condition
addPremise _ member = member

-- |
-- Applies the schema with the given name, type arguments and arguments, written in the given root.
-- Returns the members of the schema, with no includes and no schema variables, ready to be pasted in that root
applySchema :: [Root] -> [RootContext] -> RootContext -> [(Int, Identifier)] -> NameAccessChain -> [Type] -> [NamedStructExprField] -> State InlineState [SpecBlockMember]
applySchema roots contexts context stack nac typeArgs fields = do
  let resolved = fromMaybe (error $ "Included spec schema not found: " ++ show nac) (resolveName context nac)
      SchemaDecl schemaIndex schemaIdent tParams members = fromMaybe (error $ "Included spec schema not found: " ++ show nac) (findSchema roots contexts context resolved)
      schemaKey = (schemaIndex, schemaIdent)
      schemaContext = contexts !! schemaIndex
      variables = [specVariableIdentifier | SpecBlockMemberVariable SpecVariable {specVariableIdentifier} <- members]
      tParamIdents = map typeIdentifier tParams
  -- 1. Expand the nested includes, in the context of the schema's root.
  -- The checks are part of the expansion, so that they fail as soon as the result (or the state) is needed
  expanded <-
    if schemaKey `elem` stack
      then error $ "Cyclic include of spec schema: " ++ show schemaIdent
      else
        concat
          <$> forM
            (checkPureMembers schemaIdent members `seq` members)
            ( \member -> case member of
                SpecBlockMemberInclude expr -> expandSchemaExpr roots contexts schemaContext (schemaKey : stack) expr
                _ -> return [member]
            )

  -- 2. Qualify the names, so that they keep their meaning in the including root
  let locals = Set.fromList $ variables ++ tParamIdents ++ [specLetIdentifier | SpecBlockMemberLet SpecLet {specLetIdentifier} <- expanded]
      qualified = transformBi (qualifyName schemaContext (rcModule context) locals) expanded

  -- 3. Rename the schema lets to fresh names
  renamed <- renameLets qualified

  -- 4. Substitute the type parameters with the type arguments.
  -- The number of type arguments is checked even if the members contain no type
  let typeSubst =
        if length typeArgs == length tParams
          then Map.fromList (zip tParamIdents typeArgs)
          else error $ "Wrong number of type arguments for spec schema " ++ show schemaIdent ++ ": expected " ++ show (length tParams) ++ ", got " ++ show (length typeArgs)
      retyped = typeSubst `seq` transformBi (substituteType typeSubst) renamed

  -- 5. Substitute the schema variables with the arguments
  let arguments = Map.fromList $ map (fieldArgument schemaIdent variables) fields
      substituted = map (substituteInMember arguments) retyped

  -- 6. Drop the schema variables
  return [member | member <- substituted, not (isVariable member)]
  where
    -- Whether a member is a schema variable declaration
    isVariable :: SpecBlockMember -> Bool
    isVariable (SpecBlockMemberVariable _) = True
    isVariable _ = False

-- |
-- Qualifies a name written in the schema's root so that it refers to the same declaration from the including root.
-- Locals of the schema (variables, lets, type parameters) and names that do not resolve are unchanged
qualifyName :: RootContext -> Maybe ModuleKey -> Set.Set Identifier -> NameAccessChain -> NameAccessChain
qualifyName _ _ locals nac@(LocalNameAccessChain ident) | ident `Set.member` locals = nac
qualifyName schemaContext includingModule _ nac =
  case resolveName schemaContext nac of
    Nothing -> nac
    Just resolved -> qualifyFor includingModule (rcModule schemaContext) resolved

-- |
-- Converts an argument of an include to the pair (schema variable, argument expression).
-- The shorthand `{x}` stands for `{x: x}`. An argument naming a variable the schema does not declare is an error
fieldArgument :: Identifier -> [Identifier] -> NamedStructExprField -> (Identifier, Expr)
fieldArgument schemaIdent variables NamedStructExprField {nsefIdentifier, nsefExpr}
  | nsefIdentifier `notElem` variables = error $ "Spec schema " ++ show schemaIdent ++ " has no variable " ++ show nsefIdentifier
  | otherwise = (nsefIdentifier, fromMaybe (NameAccessChainExpr $ LocalNameAccessChain nsefIdentifier) nsefExpr)

-- |
-- Renames, in order, the lets of the given members to fresh names, updating the references in the following members
renameLets :: [SpecBlockMember] -> State InlineState [SpecBlockMember]
renameLets = go Map.empty
  where
    -- Renames the members with the renamings of the lets met so far
    go :: Map.Map Identifier Expr -> [SpecBlockMember] -> State InlineState [SpecBlockMember]
    go _ [] = return []
    go renamings (member : rest) = do
      let member' = substituteInMember renamings member
      case member' of
        SpecBlockMemberLet specLet@SpecLet {specLetIdentifier} -> do
          fresh <- freshIdentifier specLetIdentifier
          rest' <- go (Map.insert specLetIdentifier (NameAccessChainExpr $ LocalNameAccessChain fresh) renamings) rest
          return $ SpecBlockMemberLet specLet {specLetIdentifier = fresh} : rest'
        _ -> (member' :) <$> go renamings rest

-- |
-- Returns a fresh identifier `<name>_<k>`, not used anywhere in the project, with `k` taken from the pass counter
freshIdentifier :: Identifier -> State InlineState Identifier
freshIdentifier (Identifier name) = do
  (counter, used) <- get
  let candidates = [(k, Identifier $ name ++ "_" ++ show k) | k <- [counter ..]]
      (k', fresh) = head [c | c@(_, ident) <- candidates, not (ident `Set.member` used)]
  put (k' + 1, Set.insert fresh used)
  return fresh

-- |
-- Substitutes the type parameters in a type
substituteType :: Map.Map Identifier Type -> Type -> Type
substituteType typeSubst t@(TypeConstructor (LocalNameAccessChain ident) []) = fromMaybe t (Map.lookup ident typeSubst)
substituteType _ t = t

-- |
-- Substitutes, simultaneously, the given variables in the expressions of a member
substituteInMember :: Map.Map Identifier Expr -> SpecBlockMember -> SpecBlockMember
substituteInMember subst member
  | Map.null subst = member
  | otherwise = case member of
      SpecBlockMemberCondition condition@SpecCondition {specConditionExpr} -> SpecBlockMemberCondition condition {specConditionExpr = substituteExpr subst specConditionExpr}
      SpecBlockMemberLet specLet@SpecLet {specLetExpr} -> SpecBlockMemberLet specLet {specLetExpr = substituteExpr subst specLetExpr}
      SpecBlockMemberInclude expr -> SpecBlockMemberInclude (substituteExpr subst expr)
      SpecBlockMemberVariable _ -> member

-- |
-- Substitutes, simultaneously, the given variables in an expression, top-down, so that the substituted
-- expressions are not visited again. A struct field shorthand naming a substituted variable is expanded
substituteExpr :: Map.Map Identifier Expr -> Expr -> Expr
substituteExpr subst expr = case expr of
  NameAccessChainExpr (LocalNameAccessChain ident) | Just replacement <- Map.lookup ident subst -> replacement
  NamedStructExprExpr nse@NamedStructExpr {nseFields} -> NamedStructExprExpr nse {nseFields = map substituteField nseFields}
  _ -> descend (substituteExpr subst) expr
  where
    -- Substitutes in the expression of a field, expanding the shorthand `{x}` if `x` is substituted
    substituteField :: NamedStructExprField -> NamedStructExprField
    substituteField field@NamedStructExprField {nsefIdentifier, nsefExpr = Nothing} =
      field {nsefExpr = Map.lookup nsefIdentifier subst}
    substituteField field@NamedStructExprField {nsefExpr = Just e} = field {nsefExpr = Just $ substituteExpr subst e}

-- |
-- Checks that the members of a schema contain only expressions of the pure fragment, since the substitution
-- does not cross local binders. Throws an error otherwise
checkPureMembers :: Identifier -> [SpecBlockMember] -> ()
checkPureMembers schemaIdent members =
  case filter isImpure (universeBi members) of
    [] -> ()
    impure : _ -> error $ "Expression not supported in an included spec schema " ++ show schemaIdent ++ ": " ++ show impure
  where
    -- Whether an expression is outside the pure fragment of the specs
    isImpure :: Expr -> Bool
    isImpure e = case e of
      SequenceExpr _ -> True
      AssignmentExpr _ -> True
      WhileTerm _ -> True
      Loop _ -> True
      Return _ -> True
      Abort _ -> True
      Break -> True
      Continue -> True
      IntermediateExprExpr _ -> True
      _ -> False

-- |
-- Removes every spec schema block of a root, and the use members importing a schema of a project module
removeSchemas :: [Module] -> Root -> Root
removeSchemas userModules root = withTopLevels root (mapMaybe removeTopLevel (rootTopLevels root))
  where
    -- Drops the schemas and strips the uses
    removeTopLevel :: TopLevel -> Maybe TopLevel
    removeTopLevel (TopLevelSpecBlock SpecBlock {specBlockTarget = SpecBlockTargetSchema _ _}) = Nothing
    removeTopLevel (TopLevelUse use) = TopLevelUse <$> stripUseMembers userModules schemaNames use
    removeTopLevel topLevel = Just topLevel

    -- The names of the spec schemas declared by a module
    schemaNames :: Module -> [Identifier]
    schemaNames Module {moduleTopLevels} = [ident | TopLevelSpecBlock SpecBlock {specBlockTarget = SpecBlockTargetSchema ident _} <- moduleTopLevels]

-- |
-- Adds a module import `use A::M;` for each of the given modules that the root does not import yet
-- (and that is not the root's own module), after the existing uses
addModuleImports :: Set.Set ModuleKey -> Root -> Root
addModuleImports neededModules root =
  let topLevels = rootTopLevels root
      ownModule = case root of
        RModule Module {moduleAddress, moduleIdentifier} -> Just (moduleAddress, moduleIdentifier)
        RScript _ -> Nothing
      imported = Set.fromList [(useAddress, useIdentifier) | TopLevelUse Use {useAddress, useIdentifier} <- topLevels]
      missing = [key | key <- Set.toList neededModules, not (key `Set.member` imported), Just key /= ownModule]
      newUses = [TopLevelUse $ Use {useAddress, useIdentifier, useAlias = Nothing, useMembers = []} | (useAddress, useIdentifier) <- missing]
      (beforeLastUse, afterLastUse) = splitAfterLastUse topLevels
   in if null missing then root else withTopLevels root (beforeLastUse ++ newUses ++ afterLastUse)
  where
    -- Splits the top levels right after the last use (at the start if there are no uses)
    splitAfterLastUse :: [TopLevel] -> ([TopLevel], [TopLevel])
    splitAfterLastUse topLevels =
      let lastUseIndex = last ((-1) : [i | (i, TopLevelUse _) <- zip [0 :: Int ..] topLevels])
       in splitAt (lastUseIndex + 1) topLevels
