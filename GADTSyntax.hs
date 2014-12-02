{-# LANGUAGE GADTSyntax, 
             StandaloneDeriving, 
             DeriveFunctor,
             DeriveTraversable,
             DeriveFoldable,
             AutoDeriveTypeable -- Implies DeriveDataTypeable
             #-}

-- DONE

import Prelude hiding (Either(..), Maybe(..))
import Control.Applicative
import Data.Traversable
import Data.Foldable
import Data.Data
import Data.Typeable

-- In general, about *Making Data Types*

-- Just uses GADT Syntax
data Either a b where 
  Right :: b -> Either a b
  Left  :: a -> Either a b

-- λ> :t typeOf (Just 1)
-- typeOf (Just 1) :: TypeRep
-- λ> typeOf (Just 1)
-- Maybe Integer
-- λ> :t dataTypeOf
-- dataTypeOf :: Data a => a -> DataType
-- λ> dataTypeOf (Just 'a')
-- DataType {tycon = "Main.Maybe", datarep = AlgRep [Nothing,Just]}
-- (0.01 secs, 1548296 bytes)


--GADTSyntax.hs:18:13:
--    No instance for (Typeable Either)
--      arising from the 'deriving' clause of a data type declaration
--    Possible fix:t
--      use a standalone 'deriving instance' declaration,
--        so you can specify the instance context yourself
--    When deriving the instance for (Data (Either a b))

deriving instance Functor (Either a)
deriving instance Foldable (Either a)
deriving instance Traversable (Either a)
-- Examples of constraints
deriving instance (Show a, Show b) => Show (Either a b)

--λ> :t Right
--Right :: a -> Either a b
--λ> :t Left
--Left :: b -> Either a b

data Maybe a where
  Nothing :: Maybe a
  Just :: a -> Maybe a
  deriving (Data, Typeable)

deriving instance Show a => Show (Maybe a)
deriving instance Functor Maybe
deriving instance Foldable Maybe
deriving instance Traversable Maybe