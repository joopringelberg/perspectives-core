module TypeClassProblem where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Perspectives.CoreTypes (type (~~>), MP)
import Perspectives.DataTypeObjectGetters (binding, context, iedereRolInContext)
import Perspectives.ObjectGetterConstructors (getUnqualifiedProperty)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectivesTypesInPurescript (class Binding, class ContextType, class RolKind, class SimpleValueType, BuitenRol(..), ContextDef(..), PBool(..), RolInContext(..))
import Unsafe.Coerce (unsafeCoerce)

-- iedereRolInContext :: forall s e. ContextType s => (s ~~> RolInContext) e
-- binding :: forall rt b e. RolKind rt => Binding b => (rt ~~> b) e

-- Either give a concrete type for the function result,
s1 :: forall e. (ContextDef ~~> BuitenRol) e
s1 = iedereRolInContext /-/ binding

-- or a variable that you constrain to be a Binding - just like in 'binding' itself.
s1' :: forall b e. Binding b => (ContextDef ~~> b) e
s1' = iedereRolInContext /-/ binding

-- getUnqualifiedProperty :: forall r v e. RolKind r => SimpleValueType v => Id.LocalName -> (r ~~> v) e

-- Either provide a concrete type (PBool, here),
s2 :: forall e. (ContextDef ~~> PBool) e
s2 = s1 /-/ getUnqualifiedProperty "isFunctioneel"

-- or a type variable that is constrained to be a SimpleValueType - just like in getUnqualifiedProperty itself.
s2' :: forall v e. SimpleValueType v => (ContextDef ~~> v) e
s2' = s1 /-/ getUnqualifiedProperty "isFunctioneel"

-- However, if we now compose with s', the compiler complains that it cannot find a class instance of Binding for the result of s1'. This type variable is uninstantiated.
-- Notice that the second part of the composition, (getUnqualifiedProperty "isFunctioneel"), does
-- not provide a concrete type either as its argument is a type variable, too.
-- s2'' :: forall v e. SimpleValueType v => (ContextDef ~~> v) e
-- s2'' = s1' /-/ getUnqualifiedProperty "isFunctioneel"

-- If, however, we compose s1' with a function that provides a concrete type as argument in its type,
-- it propagates that information back into the composition and resolves the
s3 :: forall e. (ContextDef ~~> PBool) e
s3 = s1' /-/ isFunctioneel

isFunctioneel :: forall e. (BuitenRol ~~> PBool) e
isFunctioneel = getUnqualifiedProperty "isFunctioneel"

-- And we need not be concrete about the final result, either:
s3' :: forall v e. SimpleValueType v => (ContextDef ~~> v) e
s3' = s1' /-/ isFunctioneel'

isFunctioneel' :: forall v e. SimpleValueType v => (BuitenRol ~~> v) e
isFunctioneel' = getUnqualifiedProperty "isFunctioneel"

s4 :: forall v e. SimpleValueType v => (ContextDef ~~> v) e
s4 = s1 /-/ getUnqualifiedProperty "isFunctioneel"

-- This I find remarkable. Here context results in a ContextType constrained variable and
-- iedereRolInContext takes just such a constrained variable. However, without the explicit
-- annotation that there is a type variable that is, indeed, constrained by ContextType,
-- the compiler complains.
-- I assume it is just about making that 'hidden' variable explicit in the type of s5. We either
-- have to provide a type, or we have to constrain it such that it fits the other requirements.
s5 :: forall rk ct e. RolKind rk => ContextType ct => (rk ~~> RolInContext) e
s5 = (context :: (rk ~~> ct) e) /-/ iedereRolInContext

class (Newtype a String) <= T1 a
class (Newtype b String) <= T2 b
class (Newtype c String) <= T3 c

f :: forall a b. T1 a => T2 b => a -> b
f = wrap <<< unwrap

g :: forall a b. T2 a => T3 b => a -> b
g = wrap <<< unwrap

t :: forall a b c. T1 a => T2 b => T3 c => a -> c
t = (g :: b -> c) <<< f

-- It does not have to be T2, as long as we use a class that fits the bill.
-- That is, a class T4 that is a subClass of T2 does just as well.
class T2 a <= T4 a

t' :: forall a b c. T1 a => T4 b => T3 c => a -> c
t' = (g :: b -> c) <<< f

-- And an instance of T4 (and thus T2) does nicely, too:
newtype Foo = Foo String
derive instance newtypeFoo :: Newtype Foo _
instance t2Foo :: T2 Foo
instance t4Foo :: T4 Foo

t'' :: forall a c. T1 a => T3 c => a -> c
t'' = (g :: Foo -> c) <<< f

-- What I found on stackoverflow:
-- https://stackoverflow.com/questions/41088168/purescript-fails-to-match-same-constrained-types
-- https://github.com/purescript/purescript/issues/1580
