module PolyOptics where

import Control.Lens

data Promotion a = Promotion
  { _item :: a,
    _discountPercentage :: Double
  }
  deriving (Show)

item :: Lens (Promotion a) (Promotion b) a b
item = lens getter setter
  where
    getter :: Promotion a -> a
    getter = _item
    setter :: Promotion a -> b -> Promotion b
    setter promo newItem = promo {_item = newItem}