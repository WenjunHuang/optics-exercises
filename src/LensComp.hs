module LensComp where

import Control.Lens

data Payload = Payload {_weightKilos :: Int, _cargo :: String} deriving (Show)

makeLenses ''Payload

data Ship = Ship {_payload :: Payload} deriving (Show)

makeLenses ''Ship

serenity :: Ship
serenity = Ship (Payload 50000 "Livestock")

data Thermometer = Thermometer {_temperature :: Int} deriving (Show)

makeLenses ''Thermometer