module VirtualField (Temperature (..), celsiusToFahrenheit, fahrenheitToCelsius, fahrenheit,User(..),username,fullName) where

import Control.Lens

data Temperature = Temperature {_location :: String, _celsius :: Float}
  deriving (Show)

celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit c = (c * (9 / 5)) + 32

fahrenheitToCelsius :: Float -> Float
fahrenheitToCelsius f = (f - 32) * (5 / 9)

makeLenses ''Temperature

fahrenheit :: Lens' Temperature Float
fahrenheit = lens getter setter
  where
    getter = celsiusToFahrenheit . view celsius
    setter temp f = set celsius (fahrenheitToCelsius f) temp

data User = User
  { _firstName :: String,
    _lastName :: String,
    _email :: String
  } deriving (Show)

makeLenses ''User

username :: Lens' User String
username = email

fullName :: Lens' User String
fullName = lens getter setter
  where
    getter user = view firstName user <> " " <> view lastName user
    setter user name = 
      let withFirstName =set firstName (head (words name)) user
      in set lastName (unwords . tail . words $ name) withFirstName