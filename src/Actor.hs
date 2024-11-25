module Actor where
import Control.Lens 
import Data.Monoid

data Actor = Actor {_name :: String, _birthYear :: Int} deriving (Show, Eq)

makeLenses ''Actor

data TVShow = TVShow {_title :: String, _numEpisodes :: Int, _numSeasons :: Int, _criticScore :: Double, _actors :: [Actor]} deriving (Show, Eq)

makeLenses ''TVShow

howIMetYourMother :: TVShow
howIMetYourMother =
  TVShow
    { _title = "How I Met Your Mother",
      _numEpisodes = 208,
      _numSeasons = 9,
      _criticScore = 83,
      _actors =
        [ Actor "Josh Radnor" 1974,
          Actor "Cobie Smulders" 1982,
          Actor "Neil Patrick Harris" 1973,
          Actor "Alyson Hannigan" 1974,
          Actor "Jason Segel" 1980
        ]
    }

buffy :: TVShow
buffy =
  TVShow
    { _title = "Buffy the Vampire Slayer",
      _numEpisodes = 144,
      _numSeasons = 7,
      _criticScore = 81,
      _actors =
        [ Actor "Sarah Michelle Gellar" 1977,
          Actor "Alyson Hannigan" 1974,
          Actor "Nicholas Brendon" 1971,
          Actor "David Boreanaz" 1969,
          Actor "Anthony Head" 1954
        ]
    }

tvShows :: [TVShow]
tvShows = [howIMetYourMother, buffy]

calcAge :: Actor -> Int
calcAge actor = 2030 - _birthYear actor

showActor :: Actor -> String
showActor actor = _name actor <> ": " <> show (calcAge actor)

ageSummary :: Actor -> (Sum Int, Sum Int)
ageSummary actor = (Sum 1,Sum (calcAge actor))

computeAverage :: (Sum Int, Sum Int) -> Double
computeAverage (Sum count,Sum total) = fromIntegral total / fromIntegral count