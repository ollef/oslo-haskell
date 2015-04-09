module QuickCheckInstances where

import qualified Polymorphism as Poly
import Test.QuickCheck

instance Arbitrary a => Arbitrary (Poly.Tree a) where
  arbitrary = sized go
    where
      go 0 = pure Poly.Leaf
      go n = frequency
        [ (1, pure Poly.Leaf)
        , (n, Poly.Fork <$> go (n `div` 2) <*> arbitrary <*> go (n `div` 2))
        ]
  shrink Poly.Leaf           = []
  shrink (Poly.Fork t1 _ t2) = [t1, t2]

instance Arbitrary a => Arbitrary (Poly.Tree' a) where
  arbitrary = sized go
    where
      go 0 = Poly.Leaf' <$> arbitrary
      go n = frequency
        [ (1, Poly.Leaf' <$> arbitrary)
        , (n, Poly.Fork' <$> go (n `div` 2) <*> go (n `div` 2))
        ]
  shrink (Poly.Leaf' _)     = []
  shrink (Poly.Fork' t1 t2) = [t1, t2]
