{-# LANGUAGE OverloadedStrings #-}

module LensProduct (Session, userInfo, session, illegalSessionExpireTime, Builder(..), exercLaw) where

import Control.Lens (Lens', lens, makeLenses)
import Control.Lens.Unsound (lensProduct)
import Data.Text as T

type UserName = T.Text

type UserId = T.Text

data Session = Session
  { _userId :: UserId,
    _userName :: UserName,
    _createdTime :: T.Text,
    _expiryTime :: T.Text
  }
  deriving (Show, Eq)

makeLenses ''Session

userInfo :: Lens' Session (UserId, UserName)
userInfo = lensProduct userId userName

session :: Session
session = Session "User-1234" "Joey Tribbiani" "2019-07-25" "2019-08-25"

illegalSessionExpireTime :: Lens' Session T.Text
illegalSessionExpireTime = lens _expiryTime (\s t -> s {_expiryTime = t <> "illegal"})

data Builder = Builder
  { _context :: [String],
    _build :: [String] -> String
  } 

exercLaw :: Lens' Builder String
exercLaw = lens (\s -> _build s (_context s)) (\s t -> s {_build = \_ -> t})
