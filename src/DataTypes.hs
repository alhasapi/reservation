{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module DataTypes where


import Data.Yaml
import GHC.Generics

type UserName = String
type Password = String
type Language = String
type Name     = String
type Date     = String
type Hour     = Int
type Distance = Int

data User = User {
  name     :: UserName,
  password :: String
} deriving (Show, Generic)


data Guide = Guide {
  gname :: Name,
  languages :: [Language],
  busy  :: Bool
} deriving (Show, Generic)


data Car = Car {
  capacity     :: Int,
  elapsed_dist :: Distance,
  r_hour       :: Hour,
  d_hour       :: Hour,
  r_date       :: Date,
  d_date       :: Date,
  reserved     :: Bool
} deriving (Show, Generic)

data Driver = Driver {
  dname      :: Name,
  affectedTo :: Maybe Car,
  at1        :: Date
} deriving (Show, Generic)

data Reservation = Reservation {
  dWhenAndWhere :: (Date, Hour),
  rWhenAndWhere :: (Date, Hour),
  destinations  :: [(Int, String)],
  withGuides    :: Maybe [Guide],
  registedAt    :: Date,
  reserverdCars :: [(Driver, Car)],
  at            :: String
} deriving (Show, Generic)

class (Eq a) => ExEq a where
  (=<>=) :: a -> Name -> Bool


instance FromJSON Car
instance FromJSON User
instance FromJSON Guide
instance FromJSON Driver
instance FromJSON Reservation

instance ToJSON Car
instance ToJSON User
instance ToJSON Guide
instance ToJSON Driver
instance ToJSON Reservation

instance Eq User where
  (==) (User name1 pass1) (User name2 pass2)
    = (name1 == name2) && (pass1 == pass2)

instance Eq Guide where
  (==) (Guide g1 lang1 status1) (Guide g2 lang2 status2)
    = (g1      ==      g2) &&
      (lang1   ==   lang2) &&
      (status1 == status2)

instance Eq Car where
  (==) (Car a b c d e f z) (Car g h i j k l m)
    = (a == g) && (b == h) &&
      (c == i) && (d == j) &&
      (e == k) && (f == l) && (z == m)

instance Eq Driver where
  (==) (Driver n1 aff1 st1) (Driver n2 aff2 stat)
    = (n1   ==   n2) &&
      (aff2 == aff1) && (st1 == stat)

instance Eq Reservation where
  (==) (Reservation a b c d e f z) (Reservation g h i j k l y)
    =  (a == g) && (b == h) &&
       (c == i) && (d == j) &&
       (e == k) && (f == l) && (z == y)

instance ExEq User where
  (=<>=) (User n _) nam = n == nam

instance ExEq Driver where
  (=<>=) (Driver n  _ _) nam = n == nam

instance ExEq Guide where
  (=<>=) (Guide n  _ _) nam = n == nam

instance ExEq Reservation where
  (=<>=) (Reservation _ _ _ _ n _ _) nam = n == nam

-- where
--   parseJSON (Object v) = User        <$>
--                          v .:  "name" <*>
--                          v .:  "passwd"
--   parseJSON _ = error "Unable to parse User from YAML/JSON"

-- Little helper functions for loading the users, guides, drivers
-- from a data base witch is a simple yaml file

