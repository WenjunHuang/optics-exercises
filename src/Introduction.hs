{-# LANGUAGE TemplateHaskell #-}

module Introduction  where

import Control.Lens (Lens', lens, makeLenses, view)

data Ship = Ship
  { _name :: String,
    _numCrew :: Int
  }
  deriving (Show)

makeLenses ''Ship

data Pet = Pet
  { _petName :: String,
    _petType :: String
  }


makeLenses ''Pet

getPetName :: Pet -> String
getPetName pet = view petName pet

conditional :: Lens' (Bool, a, a) a
conditional = lens get set
  where
    get (True, x, _) = x
    get (False, _, y) = y
    set (True, _, y) x = (True, x, y)
    set (False, x, _) y = (False, x, y)