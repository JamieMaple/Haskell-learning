import Data.Semigroup as Sem

data Booly a = False' | True'
  deriving (Eq, Show)

-- Let's define the monadic operation for conjunction on Booly.
-- Because 'a' is phantom in Booly, we never run mappend on it.
-- Because we never run mappend on 'a', we don't need it to have an instance of
-- Monoid, and thus we don't need a constraint in the instance declaration.
instance Sem.Semigroup (Booly a) where
  (<>) False' _ = False'
  (<>) _ False' = False'
  (<>) True' True' = True'
