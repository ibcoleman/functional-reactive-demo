module Account (
  Money(..),    -- Using the (..) will export things inside the Money scope,
                  -- including the data constructors and amt
  Balance(..),
  Amount(..)
) where

import Data.Fixed

data Money = Money Centi deriving (Ord, Eq, Show)
amt (Money x) = x

instance Num Money where
  (Money a1) + (Money a2) = Money (a1 + a2 )
  (Money a1) - (Money a2) = Money (a1 - a2)
  (Money a1) * (Money a2) = Money (a1 * a2)


aMoneyValue :: Money
aMoneyValue = Money 7.83


data Foo = Foo Centi Bool deriving (Ord, Eq, Show)
bal (Foo x _) = x
happy (Foo _ x) = x

fooOne = Foo 7.3 True
fooTwo = Foo 823.32 False

-- newtype Money - the type constructor
-- Money' { amt :: Centi } deriving ... etc... - the data constructor

--data Account = Account String deriving Show -- , Eq
newtype Amount = Amount { ta :: Money} deriving (Ord, Eq, Show)
--instance Eq Amount where
--  (Amount a1) == (Amount a2) = (a1) == (a2)

newtype Balance = Balance { ba :: Money } deriving (Ord, Eq, Show)

--data AccountService = AccountService (Account, Amount, Balance) deriving Show

