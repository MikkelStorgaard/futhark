{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
-- | This module provides various simple ways to query and manipulate
-- fundamental Futhark terms, such as types and values.  The intent is to
-- keep "Futhark.Language.Syntax" simple, and put whatever embellishments
-- we need here.
module Language.Futhark.Attributes
  (
  -- * Various
    Intrinsic(..)
  , intrinsics
  , maxIntrinsicTag
  , namesToPrimTypes
  , qualName
  , qualify
  , typeName
  , valueType
  , leadingOperator
  , progImports

  -- * Queries on expressions
  , typeOf

  -- * Queries on patterns and params
  , patIdentSet
  , patternType
  , patternStructType
  , patternParam
  , patternNoShapeAnnotations
  , patternOrder

  -- * Queries on types
  , uniqueness
  , unique
  , recordArrayElemUniqueness
  , aliases
  , diet
  , arrayRank
  , nestedDims
  , returnType
  , concreteType
  , order

  -- * Operations on types
  , rank
  , peelArray
  , arrayOf
  , toStructural
  , toStruct
  , fromStruct
  , setAliases
  , addAliases
  , setUniqueness
  , modifyShapeAnnotations
  , setArrayShape
  , removeShapeAnnotations
  , vacuousShapeAnnotations
  , typeToRecordArrayElem
  , recordArrayElemToType
  , tupleRecord
  , isTupleRecord
  , areTupleFields
  , sortFields
  , isTypeParam

  -- | Values of these types are produces by the parser.  They use
  -- unadorned names and have no type information, apart from that
  -- which is syntactically required.
  , NoInfo(..)
  , UncheckedType
  , UncheckedTypeExp
  , UncheckedArrayElemType
  , UncheckedIdent
  , UncheckedTypeDecl
  , UncheckedDimIndex
  , UncheckedExp
  , UncheckedModExp
  , UncheckedSigExp
  , UncheckedTypeParam
  , UncheckedPattern
  , UncheckedValBind
  , UncheckedDec
  , UncheckedProg
  )
  where

import           Control.Monad.Writer
import           Data.Foldable
import qualified Data.Map.Strict       as M
import qualified Data.Set            as S
import           Data.List
import           Data.Loc
import           Data.Maybe
import           Data.Ord
import           Data.Bifunctor
import           Data.Bifoldable

import           Prelude

import           Futhark.Util.Pretty

import           Language.Futhark.Syntax
import qualified Futhark.Representation.Primitive as Primitive

-- | Return the dimensionality of a type.  For non-arrays, this is
-- zero.  For a one-dimensional array it is one, for a two-dimensional
-- it is two, and so forth.
arrayRank :: ArrayDim dim =>
             TypeBase dim as -> Int
arrayRank = shapeRank . arrayShape

-- | Return the shape of a type - for non-arrays, this is 'mempty'.
arrayShape :: TypeBase dim as -> ShapeDecl dim
arrayShape (Array _ ds _) = ds
arrayShape _ = mempty

-- | Return any shape declarations in the type, with duplicates
-- removed.
nestedDims :: TypeBase (DimDecl VName) as -> [DimDecl VName]
nestedDims t =
  case t of Array a ds _    -> nub $ arrayNestedDims a <> shapeDims ds
            Record fs       -> nub $ fold $ fmap nestedDims fs
            Prim{}          -> mempty
            TypeVar _ targs -> concatMap typeArgDims targs
            Arrow _ v t1 t2 -> filter (notV v) $ nestedDims t1 <> nestedDims t2
  where arrayNestedDims ArrayPrimElem{} =
          mempty
        arrayNestedDims (ArrayPolyElem _ targs _) =
          concatMap typeArgDims targs
        arrayNestedDims (ArrayRecordElem ts) =
          fold (fmap recordArrayElemNestedDims ts)

        recordArrayElemNestedDims (RecordArrayArrayElem a ds _) =
          arrayNestedDims a <> shapeDims ds
        recordArrayElemNestedDims (RecordArrayElem et) =
          arrayNestedDims et

        typeArgDims (TypeArgDim d _) = [d]
        typeArgDims (TypeArgType at _) = nestedDims at

        notV Nothing  = const True
        notV (Just v) = (/=NamedDim (qualName v))

-- | Set the dimensions of an array.  If the given type is not an
-- array, return the type unchanged.
setArrayShape :: TypeBase dim as -> ShapeDecl dim -> TypeBase dim as
setArrayShape (Array t _ u) ds = Array t ds u
setArrayShape t _ = t

-- | Change the shape of a type to be just the 'Rank'.
removeShapeAnnotations :: TypeBase dim as -> TypeBase () as
removeShapeAnnotations = modifyShapeAnnotations $ const ()

-- | Change all size annotations to be 'AnyDim'.
vacuousShapeAnnotations :: TypeBase dim as -> TypeBase (DimDecl vn) as
vacuousShapeAnnotations = modifyShapeAnnotations $ const AnyDim

-- | Change the size annotations of a type.
modifyShapeAnnotations :: (oldshape -> newshape)
                       -> TypeBase oldshape as
                       -> TypeBase newshape as
modifyShapeAnnotations f = bimap f id

-- | Return the uniqueness of a type.
uniqueness :: TypeBase shape as -> Uniqueness
uniqueness (Array _ _ u) = u
uniqueness _ = Nonunique

recordArrayElemUniqueness :: RecordArrayElemTypeBase shape as -> Uniqueness
recordArrayElemUniqueness RecordArrayElem{} = Nonunique
recordArrayElemUniqueness (RecordArrayArrayElem _ _ u) = u

-- | @unique t@ is 'True' if the type of the argument is unique.
unique :: TypeBase shape as -> Bool
unique = (==Unique) . uniqueness

-- | Return the set of all variables mentioned in the aliasing of a
-- type.
aliases :: Monoid as => TypeBase shape as -> as
aliases = bifoldMap (const mempty) id

-- | @diet t@ returns a description of how a function parameter of
-- type @t@ might consume its argument.
diet :: TypeBase shape as -> Diet
diet (Record ets)          = RecordDiet $ fmap diet ets
diet (Prim _)              = Observe
diet TypeVar{}             = Observe
diet Arrow{}               = Observe
diet (Array _ _ Unique)    = Consume
diet (Array _ _ Nonunique) = Observe

-- | @t `maskAliases` d@ removes aliases (sets them to 'mempty') from
-- the parts of @t@ that are denoted as 'Consumed' by the 'Diet' @d@.
maskAliases :: Monoid as =>
               TypeBase shape as
            -> Diet
            -> TypeBase shape as
maskAliases t Consume = t `setAliases` mempty
maskAliases t Observe = t
maskAliases (Record ets) (RecordDiet ds) =
  Record $ M.intersectionWith maskAliases ets ds
maskAliases _ _ = error "Invalid arguments passed to maskAliases."

-- | Convert any type to one that has rank information, no alias
-- information, and no embedded names.
toStructural :: ArrayDim dim =>
                TypeBase dim as
             -> TypeBase () ()
toStructural = removeNames . removeShapeAnnotations

-- | Remove aliasing information from a type.
toStruct :: TypeBase dim as
         -> TypeBase dim ()
toStruct t = t `setAliases` ()

-- | Replace no aliasing with an empty alias set.
fromStruct :: TypeBase dim as
           -> TypeBase dim Names
fromStruct t = t `setAliases` S.empty

-- | @peelArray n t@ returns the type resulting from peeling the first
-- @n@ array dimensions from @t@.  Returns @Nothing@ if @t@ has less
-- than @n@ dimensions.
peelArray :: ArrayDim dim =>
             Int -> TypeBase dim as -> Maybe (TypeBase dim as)
peelArray 0 t = Just t
peelArray n (Array (ArrayPrimElem et _) shape _)
  | shapeRank shape == n =
    Just $ Prim et
peelArray n (Array (ArrayPolyElem et targs _) shape _)
  | shapeRank shape == n =
    Just $ TypeVar et targs
peelArray n (Array (ArrayRecordElem ts) shape _)
  | shapeRank shape == n =
    Just $ Record $ fmap asType ts
  where asType (RecordArrayElem (ArrayPrimElem bt _)) = Prim bt
        asType (RecordArrayElem (ArrayPolyElem bt targs _)) = TypeVar bt targs
        asType (RecordArrayElem (ArrayRecordElem ts'))  = Record $ fmap asType ts'
        asType (RecordArrayArrayElem et e_shape u) = Array et e_shape u
peelArray n (Array et shape u) = do
  shape' <- stripDims n shape
  return $ Array et shape' u
peelArray _ _ = Nothing

-- | Remove names from a type - this involves removing all size
-- annotations from arrays, as well as all aliasing.
removeNames :: ArrayDim dim =>
               TypeBase dim as
            -> TypeBase () ()
removeNames = flip setAliases () . removeShapeAnnotations

-- | @arrayOf t s u@ constructs an array type.  The convenience
-- compared to using the 'Array' constructor directly is that @t@ can
-- itself be an array.  If @t@ is an @n@-dimensional array, and @s@ is
-- a list of length @n@, the resulting type is of an @n+m@ dimensions.
-- The uniqueness of the new array will be @u@, no matter the
-- uniqueness of @t@.  The function returns 'Nothing' in case an
-- attempt is made to create an array of functions.
arrayOf :: (ArrayDim dim, Monoid as) =>
           TypeBase dim as
        -> ShapeDecl dim
        -> Uniqueness
        -> Maybe (TypeBase dim as)
arrayOf (Array et shape1 _) shape2 u =
  Just $ Array et (shape2 <> shape1) u
arrayOf (Prim et) shape u =
  Just $ Array (ArrayPrimElem et mempty) shape u
arrayOf (TypeVar x targs) shape u =
  Just $ Array (ArrayPolyElem x targs mempty) shape u
arrayOf (Record ts) shape u = do
  ts' <- traverse typeToRecordArrayElem ts
  return $ Array (ArrayRecordElem ts') shape u
arrayOf Arrow{} _ _ = Nothing

typeToRecordArrayElem :: Monoid as =>
                         TypeBase dim as
                      -> Maybe (RecordArrayElemTypeBase dim as)
typeToRecordArrayElem (Prim bt) =
  Just $ RecordArrayElem $ ArrayPrimElem bt mempty
typeToRecordArrayElem (TypeVar bt targs) =
  Just $ RecordArrayElem $ ArrayPolyElem bt targs mempty
typeToRecordArrayElem (Record ts') =
  RecordArrayElem . ArrayRecordElem <$> traverse typeToRecordArrayElem ts'
typeToRecordArrayElem (Array et shape u) =
  Just $ RecordArrayArrayElem et shape u
typeToRecordArrayElem Arrow{} = Nothing

recordArrayElemToType :: RecordArrayElemTypeBase dim as
                     -> TypeBase dim as
recordArrayElemToType (RecordArrayElem et)              = arrayElemToType et
recordArrayElemToType (RecordArrayArrayElem et shape u) = Array et shape u

arrayElemToType :: ArrayElemTypeBase dim as -> TypeBase dim as
arrayElemToType (ArrayPrimElem bt _)       = Prim bt
arrayElemToType (ArrayPolyElem bt targs _) = TypeVar bt targs
arrayElemToType (ArrayRecordElem ts)       = Record $ fmap recordArrayElemToType ts

-- | @stripArray n t@ removes the @n@ outermost layers of the array.
-- Essentially, it is the type of indexing an array of type @t@ with
-- @n@ indexes.
stripArray :: ArrayDim dim =>
              Int -> TypeBase dim as -> TypeBase dim as
stripArray n (Array et shape u)
  | Just shape' <- stripDims n shape =
    Array et shape' u
  | otherwise = arrayElemToType et
stripArray _ t = t

-- | Create a record type corresponding to a tuple with the given
-- element types.
tupleRecord :: [TypeBase dim as] -> TypeBase dim as
tupleRecord = Record . M.fromList . zip tupleFieldNames

isTupleRecord :: TypeBase dim as -> Maybe [TypeBase dim as]
isTupleRecord (Record fs) = areTupleFields fs
isTupleRecord _ = Nothing

areTupleFields :: M.Map Name a -> Maybe [a]
areTupleFields fs =
  let fs' = sortFields fs
  in if and $ zipWith (==) (map fst fs') tupleFieldNames
     then Just $ map snd fs'
     else Nothing

-- | Increasing field names for a tuple (starts at 1).
tupleFieldNames :: [Name]
tupleFieldNames = map (nameFromString . show) [(1::Int)..]

-- | Sort fields by their name; taking care to sort numeric fields by
-- their numeric value.  This ensures that tuples and tuple-like
-- records match.
sortFields :: M.Map Name a -> [(Name,a)]
sortFields l = map snd $ sortBy (comparing fst) $ zip (map (fieldish . fst) l') l'
  where l' = M.toList l
        fieldish s = case reads $ nameToString s of
          [(x, "")] -> Left (x::Int)
          _         -> Right s

isTypeParam :: TypeParamBase vn -> Bool
isTypeParam TypeParamType{} = True
isTypeParam TypeParamDim{}  = False


-- | Set the uniqueness attribute of a type.  If the type is a tuple,
-- the uniqueness of its components will be modified.
setUniqueness :: TypeBase dim as -> Uniqueness -> TypeBase dim as
setUniqueness (Array et shape _) u =
  Array (setArrayElemUniqueness et u) shape u
setUniqueness (Record ets) u =
  Record $ fmap (`setUniqueness` u) ets
setUniqueness t _ = t

setArrayElemUniqueness :: ArrayElemTypeBase dim as
                       -> Uniqueness -> ArrayElemTypeBase dim as
setArrayElemUniqueness (ArrayPrimElem bt as) _ =
  ArrayPrimElem bt as
setArrayElemUniqueness (ArrayPolyElem v args as) _ =
  ArrayPolyElem v args as
setArrayElemUniqueness (ArrayRecordElem r) u =
  ArrayRecordElem $ fmap set r
  where set (RecordArrayElem et) =
          RecordArrayElem $ setArrayElemUniqueness et u
        set (RecordArrayArrayElem et shape e_u) =
          RecordArrayArrayElem (setArrayElemUniqueness et u) shape e_u

-- | @t \`setAliases\` als@ returns @t@, but with @als@ substituted for
-- any already present aliasing.
setAliases :: TypeBase dim asf -> ast -> TypeBase dim ast
setAliases t = addAliases t . const

-- | @t \`addAliases\` f@ returns @t@, but with any already present
-- aliasing replaced by @f@ applied to that aliasing.
addAliases :: TypeBase dim asf -> (asf -> ast)
           -> TypeBase dim ast
addAliases t f = bimap id f t

intValueType :: IntValue -> IntType
intValueType Int8Value{}  = Int8
intValueType Int16Value{} = Int16
intValueType Int32Value{} = Int32
intValueType Int64Value{} = Int64

floatValueType :: FloatValue -> FloatType
floatValueType Float32Value{} = Float32
floatValueType Float64Value{} = Float64

-- | The type of a basic value.
primValueType :: PrimValue -> PrimType
primValueType (SignedValue v)   = Signed $ intValueType v
primValueType (UnsignedValue v) = Unsigned $ intValueType v
primValueType (FloatValue v)    = FloatType $ floatValueType v
primValueType BoolValue{}       = Bool

valueType :: Value -> TypeBase () ()
valueType (PrimValue bv) = Prim $ primValueType bv
valueType (ArrayValue _ t) = t

-- | Construct a 'ShapeDecl' with the given number of zero-information
-- dimensions.
rank :: Int -> ShapeDecl ()
rank n = ShapeDecl $ replicate n ()

-- | The type of an Futhark term.  The aliasing will refer to itself, if
-- the term is a non-tuple-typed variable.
--
-- HACK: For terms that are really in a function position (such as the
-- functional argument to a @Map@), the type will be the *return
-- type*, even if the function is not fully applied.  This will change
-- once Futhark gets proper support for higher-order functions.
typeOf :: ExpBase Info VName -> CompType
typeOf (Literal val _) = Prim $ primValueType val
typeOf (Parens e _) = typeOf e
typeOf (QualParens _ e _) = typeOf e
typeOf (TupLit es _) = tupleRecord $ map typeOf es
typeOf (RecordLit fs _) =
  -- Reverse, because M.unions is biased to the left.
  Record $ M.unions $ reverse $ map record fs
  where record (RecordFieldExplicit name e _) = M.singleton name $ typeOf e
        record (RecordFieldImplicit name (Info t) _) = M.singleton (baseName name) t
typeOf (ArrayLit _ (Info t) _) = t
typeOf (Range _ _ _ (Info t) _) = t
typeOf (Empty _ (Info t) _) = t
typeOf (BinOp _ _ _ (Info t) _) = t
typeOf (Project _ _ (Info t) _) = t
typeOf (If _ _ _ (Info t) _) = t
typeOf (Var _ (Info (_, ts, ret@Record{})) _) =
  foldFunType ts ret
typeOf (Var qn (Info (_, ts, ret)) _) =
  foldFunType ts (ret `addAliases` S.insert (qualLeaf qn))
typeOf (Ascript e _ _) = typeOf e
typeOf (Apply _ _ _ (Info (ts, ret)) _) =
  foldFunType ts ret
typeOf (Negate e _) = typeOf e
typeOf (LetPat _ _ _ body _) = typeOf body
typeOf (LetFun _ _ body _) = typeOf body
typeOf (LetWith _ _ _ _ body _) = typeOf body
typeOf (Index ident idx _) =
  stripArray (length $ filter isFix idx) (typeOf ident)
  where isFix DimFix{} = True
        isFix _        = False
typeOf (Update e _ _ _) = typeOf e `setAliases` mempty
typeOf (Reshape shape  e _) =
  typeOf e `setArrayShape` rank n
  where n = case typeOf shape of Record ts -> length ts
                                 _         -> 1
typeOf (Rearrange _ e _) = typeOf e
typeOf (Rotate _ _ e _) = typeOf e
typeOf (Zip i _ _ (Info ts) (Info u) _) =
  Array (ArrayRecordElem $ M.fromList $ zip tupleFieldNames ts) (rank (1+i)) u
typeOf (Unzip _ ts _) =
  tupleRecord $ map unInfo ts
typeOf (Unsafe e _) =
  typeOf e
typeOf (Map _ _ (Info t) _) = t
typeOf (Reduce _ _ _ arr _) =
  stripArray 1 (typeOf arr) `setAliases` mempty
typeOf (Scan _ _ arr _) = typeOf arr `setAliases` mempty
typeOf (Filter _ arr _) = typeOf arr
typeOf (Partition funs arr _) =
  tupleRecord $ replicate (length funs + 1) $ typeOf arr
typeOf (Stream form lam _ _) =
  case form of
    MapLike{}    -> typeOf lam `setUniqueness` Unique
    RedLike{}    -> typeOf lam `setUniqueness` Unique
typeOf (Concat _ x _ _) =
  typeOf x `setUniqueness` Unique `setAliases` S.empty
typeOf (DoLoop _ pat _ _ _ _) = patternType pat
typeOf (Lambda _ _ _ _ (Info t) _) =
  removeShapeAnnotations t `setAliases` mempty
typeOf (OpSection _ _ _ (Info t) _)      = toStruct t `setAliases` mempty
typeOf (OpSectionLeft _ _ _ (Info t) _)  = toStruct t `setAliases` mempty
typeOf (OpSectionRight _ _ _ (Info t) _) = toStruct t `setAliases` mempty

foldFunType :: [StructType] -> CompType -> CompType
foldFunType ps ret =
  foldr (Arrow mempty Nothing . removeShapeAnnotations . fromStruct) ret ps

-- | The result of applying the arguments of the given types to a
-- function with the given return type, consuming its parameters with
-- the given diets.
returnType :: TypeBase dim ()
           -> [Diet]
           -> [CompType]
           -> TypeBase dim Names
returnType (Array et shape Unique) _ _ =
  Array (bimap id (const mempty) et) shape Unique
returnType (Array et shape Nonunique) ds args =
  Array (arrayElemReturnType et ds args) shape Nonunique
returnType (Record fs) ds args =
  Record $ fmap (\et -> returnType et ds args) fs
returnType (Prim t) _ _ = Prim t
returnType (TypeVar t targs) ds args =
  TypeVar t $ map (\arg -> typeArgReturnType arg ds args) targs
returnType (Arrow _ v t1 t2) ds args =
  Arrow mempty v (bimap id (const mempty) t1) (returnType t2 ds args)

typeArgReturnType :: TypeArg shape () -> [Diet] -> [CompType]
                  -> TypeArg shape Names
typeArgReturnType (TypeArgDim v loc) _ _ =
  TypeArgDim v loc
typeArgReturnType (TypeArgType t loc) ds args =
  TypeArgType (returnType t ds args) loc

arrayElemReturnType :: ArrayElemTypeBase dim ()
                    -> [Diet]
                    -> [CompType]
                    -> ArrayElemTypeBase dim Names
arrayElemReturnType (ArrayPrimElem bt ()) ds args =
  ArrayPrimElem bt als
  where als = mconcat $ map aliases $ zipWith maskAliases args ds
arrayElemReturnType (ArrayPolyElem bt targs ()) ds args =
  ArrayPolyElem bt (map (\arg -> typeArgReturnType arg ds args) targs) als
  where als = mconcat $ map aliases $ zipWith maskAliases args ds
arrayElemReturnType (ArrayRecordElem et) ds args =
  ArrayRecordElem $ fmap (\t -> recordArrayElemReturnType t ds args) et

recordArrayElemReturnType :: RecordArrayElemTypeBase dim ()
                         -> [Diet]
                         -> [CompType]
                         -> RecordArrayElemTypeBase dim Names
recordArrayElemReturnType (RecordArrayElem et) ds args =
  RecordArrayElem $ arrayElemReturnType et ds args
recordArrayElemReturnType (RecordArrayArrayElem et shape u) ds args =
  RecordArrayArrayElem (arrayElemReturnType et ds args) shape u

-- | Is the type concrete, i.e, without any type variables or function arrows?
concreteType :: TypeBase f vn -> Bool
concreteType Prim{} = True
concreteType TypeVar{} = False
concreteType Arrow{} = False
concreteType (Record ts) = all concreteType ts
concreteType (Array at _ _) = concreteArrayType at
  where concreteArrayType ArrayPrimElem{}      = True
        concreteArrayType ArrayPolyElem{}      = False
        concreteArrayType (ArrayRecordElem ts) = all concreteRecordArrayElem ts

        concreteRecordArrayElem (RecordArrayElem et) = concreteArrayType et
        concreteRecordArrayElem (RecordArrayArrayElem et _ _) = concreteArrayType et

-- | Return the order of a type. Primitive types and arrays have order 0,
-- while types containing function types have order at least 1.
order :: TypeBase dim as -> Int
order (Prim _)          = 0
order Array{}           = 0
order (Record fs)       = M.foldl' (flip $ max . order) 0 fs
order TypeVar{}         = 0
order (Arrow _ _ t1 t2) = order t1 + 1 `max` order t2

-- | The set of identifiers bound in a pattern.
patIdentSet :: (Functor f, Ord vn) => PatternBase f vn -> S.Set (IdentBase f vn)
patIdentSet (Id v t loc)            = S.singleton $ Ident v (removeShapeAnnotations <$> t) loc
patIdentSet (PatternParens p _)     = patIdentSet p
patIdentSet (TuplePattern pats _)   = mconcat $ map patIdentSet pats
patIdentSet (RecordPattern fs _)    = mconcat $ map (patIdentSet . snd) fs
patIdentSet Wildcard{}              = mempty
patIdentSet (PatternAscription p _) = patIdentSet p

-- | The type of values bound by the pattern.
patternType :: PatternBase Info VName -> CompType
patternType (Wildcard (Info t) _)   = removeShapeAnnotations t
patternType (PatternParens p _)     = patternType p
patternType (Id _ (Info t) _)       = removeShapeAnnotations t
patternType (TuplePattern pats _)   = tupleRecord $ map patternType pats
patternType (RecordPattern fs _)    = Record $ patternType <$> M.fromList fs
patternType (PatternAscription p _) = patternType p

-- | The type matched by the pattern, including shape declarations if present.
patternStructType :: PatternBase Info VName -> StructType
patternStructType (PatternAscription _ td) = unInfo $ expandedType td
patternStructType (PatternParens p _) = patternStructType p
patternStructType (Id _ (Info t) _) = t `setAliases` ()
patternStructType (TuplePattern ps _) = tupleRecord $ map patternStructType ps
patternStructType (RecordPattern fs _) = Record $ patternStructType <$> M.fromList fs
patternStructType (Wildcard (Info t) _) = vacuousShapeAnnotations $ toStruct t

-- | When viewed as a function parameter, does this pattern correspond
-- to a named parameter of some type?
patternParam :: PatternBase Info VName -> (Maybe VName, StructType)
patternParam (PatternParens p _) =
  patternParam p
patternParam (PatternAscription (Id v _ _) td) =
  (Just v, unInfo $ expandedType td)
patternParam p =
  (Nothing, patternStructType p)

-- | Remove all shape annotations from a pattern, leaving them unnamed
-- instead.
patternNoShapeAnnotations :: PatternBase Info VName -> PatternBase Info VName
patternNoShapeAnnotations (PatternAscription p (TypeDecl te (Info t))) =
  PatternAscription (patternNoShapeAnnotations p) $
  TypeDecl te $ Info $ vacuousShapeAnnotations t
patternNoShapeAnnotations (PatternParens p loc) =
  PatternParens (patternNoShapeAnnotations p) loc
patternNoShapeAnnotations (Id v (Info t) loc) =
  Id v (Info $ vacuousShapeAnnotations t) loc
patternNoShapeAnnotations (TuplePattern ps loc) =
  TuplePattern (map patternNoShapeAnnotations ps) loc
patternNoShapeAnnotations (RecordPattern ps loc) =
  RecordPattern (map (fmap patternNoShapeAnnotations) ps) loc
patternNoShapeAnnotations (Wildcard (Info t) loc) =
  Wildcard (Info (vacuousShapeAnnotations t)) loc

-- | Compute the maximum order of a type that occurs in a given pattern.
patternOrder :: PatternBase Info vn -> Int
patternOrder pat = case pat of
  TuplePattern ps _     -> foldl' max 0 $ map patternOrder ps
  RecordPattern fs _    -> foldl' max 0 $ map (patternOrder . snd) fs
  PatternParens p _     -> patternOrder p
  Id _ (Info t) _       -> order t
  Wildcard (Info t) _   -> order t
  PatternAscription p _ -> patternOrder p

-- | Names of primitive types to types.  This is only valid if no
-- shadowing is going on, but useful for tools.
namesToPrimTypes :: M.Map Name PrimType
namesToPrimTypes = M.fromList
                   [ (nameFromString $ pretty t, t) |
                     t <- Bool :
                          map Signed [minBound..maxBound] ++
                          map Unsigned [minBound..maxBound] ++
                          map FloatType [minBound..maxBound] ]

-- | The nature of something predefined.  These can either be monomorphic
-- or overloaded.  An overloaded builtin is a mapping from valid
-- parameter types to the result type.
data Intrinsic = IntrinsicMonoFun [PrimType] PrimType
               | IntrinsicOverloadedFun [(PrimType, ([PrimType], PrimType))]
               | IntrinsicPolyFun [TypeParamBase VName] [TypeBase () ()] (TypeBase () ())
               | IntrinsicType PrimType
               | IntrinsicEquality -- Special cased.
               | IntrinsicOpaque

-- | A map of all built-ins.
intrinsics :: M.Map VName Intrinsic
intrinsics = M.fromList $ zipWith namify [10..] $

             map primFun (M.toList Primitive.primFuns) ++

             [ ("~", IntrinsicOverloadedFun $
                     [(Signed t, ([Signed t], Signed t)) | t <- [minBound..maxBound] ] ++
                     [(Unsigned t, ([Unsigned t], Unsigned t)) | t <- [minBound..maxBound] ])
             , ("!", IntrinsicMonoFun [Bool] Bool)] ++

             [("opaque", IntrinsicOpaque)] ++

             map unOpFun Primitive.allUnOps ++

             map binOpFun Primitive.allBinOps ++

             map cmpOpFun Primitive.allCmpOps ++

             map convOpFun Primitive.allConvOps ++

             map signFun Primitive.allIntTypes ++

             map unsignFun Primitive.allIntTypes ++

             map intrinsicType (map Signed [minBound..maxBound] ++
                                map Unsigned [minBound..maxBound] ++
                                map FloatType [minBound..maxBound] ++
                                [Bool]) ++

             -- The reason for the loop formulation is to ensure that we
             -- get a missing case warning if we forget a case.
             map mkIntrinsicBinOp [minBound..maxBound] ++

             [("scatter", IntrinsicPolyFun [tp_a]
                          [Array (ArrayPolyElem tv_a' [] ()) (rank 1) Unique,
                           Array (ArrayPrimElem (Signed Int32) ()) (rank 1) Nonunique,
                           Array (ArrayPolyElem tv_a' [] ()) (rank 1) Nonunique] $
                          Array (ArrayPolyElem tv_a' [] ()) (rank 1) Unique)]

  where tv_a = VName (nameFromString "a") 0
        tv_a' = typeName tv_a
        tp_a = TypeParamType tv_a noLoc

        namify i (k,v) = (VName (nameFromString k) i, v)

        primFun (name, (ts,t, _)) =
          (name, IntrinsicMonoFun (map unPrim ts) $ unPrim t)

        unOpFun bop = (pretty bop, IntrinsicMonoFun [t] t)
          where t = unPrim $ Primitive.unOpType bop

        binOpFun bop = (pretty bop, IntrinsicMonoFun [t, t] t)
          where t = unPrim $ Primitive.binOpType bop

        cmpOpFun bop = (pretty bop, IntrinsicMonoFun [t, t] Bool)
          where t = unPrim $ Primitive.cmpOpType bop

        convOpFun cop = (pretty cop, IntrinsicMonoFun [unPrim ft] $ unPrim tt)
          where (ft, tt) = Primitive.convOpType cop

        signFun t = ("sign_" ++ pretty t, IntrinsicMonoFun [Unsigned t] $ Signed t)

        unsignFun t = ("unsign_" ++ pretty t, IntrinsicMonoFun [Signed t] $ Unsigned t)

        unPrim (Primitive.IntType t) = Signed t
        unPrim (Primitive.FloatType t) = FloatType t
        unPrim Primitive.Bool = Bool
        unPrim Primitive.Cert = Bool

        intrinsicType t = (pretty t, IntrinsicType t)

        anyIntType = map Signed [minBound..maxBound] ++
                     map Unsigned [minBound..maxBound]
        anyNumberType = anyIntType ++
                        map FloatType [minBound..maxBound]
        anyPrimType = Bool : anyNumberType

        mkIntrinsicBinOp :: BinOp -> (String, Intrinsic)
        mkIntrinsicBinOp op = (pretty op, intrinsicBinOp op)

        binOp :: [PrimType] -> Intrinsic
        binOp ts = IntrinsicOverloadedFun [ (t, ([t,t], t)) | t <- ts ]

        intrinsicBinOp Plus     = binOp anyNumberType
        intrinsicBinOp Minus    = binOp anyNumberType
        intrinsicBinOp Pow      = binOp anyNumberType
        intrinsicBinOp Times    = binOp anyNumberType
        intrinsicBinOp Divide   = binOp anyNumberType
        intrinsicBinOp Mod      = binOp anyNumberType
        intrinsicBinOp Quot     = binOp anyIntType
        intrinsicBinOp Rem      = binOp anyIntType
        intrinsicBinOp ShiftR   = binOp anyIntType
        intrinsicBinOp ZShiftR  = binOp anyIntType
        intrinsicBinOp ShiftL   = binOp anyIntType
        intrinsicBinOp Band     = binOp anyIntType
        intrinsicBinOp Xor      = binOp anyIntType
        intrinsicBinOp Bor      = binOp anyIntType
        intrinsicBinOp LogAnd   = IntrinsicMonoFun [Bool,Bool] Bool
        intrinsicBinOp LogOr    = IntrinsicMonoFun [Bool,Bool] Bool
        intrinsicBinOp Equal    = IntrinsicEquality
        intrinsicBinOp NotEqual = IntrinsicEquality
        intrinsicBinOp Less     = ordering
        intrinsicBinOp Leq      = ordering
        intrinsicBinOp Greater  = ordering
        intrinsicBinOp Geq      = ordering

        ordering = IntrinsicOverloadedFun [ (t, ([t,t], Bool)) | t <- anyPrimType ]

-- | The largest tag used by an intrinsic - this can be used to
-- determine whether a 'VName' refers to an intrinsic or a user-defined name.
maxIntrinsicTag :: Int
maxIntrinsicTag = maximum $ map baseTag $ M.keys intrinsics

-- | Create a name with no qualifiers from a name.
qualName :: v -> QualName v
qualName = QualName []

-- | Add another qualifier (at the head) to a qualified name.
qualify :: v -> QualName v -> QualName v
qualify k (QualName ks v) = QualName (k:ks) v

-- | Create a type name name with no qualifiers from a 'VName'.
typeName :: VName -> TypeName
typeName = typeNameFromQualName . qualName

progImports :: ProgBase f vn -> [(String,SrcLoc)]
progImports = concatMap decImports . progDecs
  where decImports (OpenDec x xs _ _) =
          concatMap modExpImports $ x:xs
        decImports (ModDec md) =
          modExpImports $ modExp md
        decImports SigDec{} = []
        decImports TypeDec{} = []
        decImports ValDec{} = []
        decImports (LocalDec d _) = decImports d

        modExpImports ModVar{}              = []
        modExpImports (ModParens p _)       = modExpImports p
        modExpImports (ModImport f _ loc)   = [(f,loc)]
        modExpImports (ModDecs ds _)        = concatMap decImports ds
        modExpImports (ModApply _ me _ _ _) = modExpImports me
        modExpImports (ModAscript me _ _ _) = modExpImports me
        modExpImports ModLambda{}           = []

-- | Given an operator name, return the operator that determines its
-- syntactical properties.
leadingOperator :: Name -> BinOp
leadingOperator s = maybe LogAnd snd $ find ((`isPrefixOf` s') . fst) $
                    sortBy (flip $ comparing $ length . fst) $
                    zip (map pretty operators) operators
  where s' = nameToString s
        operators :: [BinOp]
        operators = [minBound..maxBound::BinOp]

-- | A type with no aliasing information but shape annotations.
type UncheckedType = TypeBase (ShapeDecl Name) ()

type UncheckedTypeExp = TypeExp Name

-- | An array element type with no aliasing information.
type UncheckedArrayElemType = ArrayElemTypeBase (ShapeDecl Name) ()

-- | A type declaration with no expanded type.
type UncheckedTypeDecl = TypeDeclBase NoInfo Name

-- | An identifier with no type annotations.
type UncheckedIdent = IdentBase NoInfo Name

-- | An index with no type annotations.
type UncheckedDimIndex = DimIndexBase NoInfo Name

-- | An expression with no type annotations.
type UncheckedExp = ExpBase NoInfo Name

-- | A module expression with no type annotations.
type UncheckedModExp = ModExpBase NoInfo Name

-- | A module type expression with no type annotations.
type UncheckedSigExp = SigExpBase NoInfo Name

-- | A type parameter with no type annotations.
type UncheckedTypeParam = TypeParamBase Name

-- | A pattern with no type annotations.
type UncheckedPattern = PatternBase NoInfo Name

-- | A function declaration with no type annotations.
type UncheckedValBind = ValBindBase NoInfo Name

-- | A declaration with no type annotations.
type UncheckedDec = DecBase NoInfo Name

-- | A Futhark program with no type annotations.
type UncheckedProg = ProgBase NoInfo Name
