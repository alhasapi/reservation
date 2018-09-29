{-# LANGUAGE OverloadedStrings #-}

module DB (
  loadUsersDB,
  loadCarsDB,
  loadDriversDB,
  loadGuidesDB,
  loadReservationDB,
  loadObj,
  LoaderInterface,
  deleteUser,
  deleteGuide,
  deleteReservation,
  deleteCar,
  deleteDriver,
  addUser,
  addCar,
  addDriver,
  addGuide,
  addReservation,
  assignCarCapacity,
  assignCarDist,
  assignReservationDateDeDarrive,
  assignReservationDestination,
  assignReservationGuides,
  assignReservationDateDeDepart,
  showCarsList,
  showDrivers,
  pivot,
  sHead,
  showLine,
  ask,
  lookupForCar
) where

import Data.Yaml
import DataTypes
import System.IO
import Control.Monad

import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as BS


class LoaderInterface a where
  loadObj :: (FromJSON a, ToJSON a) => IO (Maybe [a])
  theDate :: (FromJSON a, ToJSON a) => a -> Date

instance LoaderInterface User where
  loadObj = loadUsersDB
  theDate = undefined

instance LoaderInterface Guide where
  loadObj = loadGuidesDB
  theDate = undefined

instance LoaderInterface Driver where
  loadObj = loadDriversDB
  theDate (Driver _ _ d) = d

instance LoaderInterface Car where
  loadObj = loadCarsDB
  theDate (Car _ _ _ _ _ d _ ) = d

instance LoaderInterface Reservation where
  loadObj = loadReservationDB
  theDate (Reservation _ _ _ _ d _ _) = d

driversDB, usersDB, carsDB, guidesDB, reservationsDB :: FilePath

usersDB   = "dbFiles/users.yml"
carsDB    = "dbFiles/cars.yml"
guidesDB  = "dbFiles/guides.yml"
driversDB = "dbFiles/drivers.yml"
reservationsDB = "dbFiles/reservations.yml"


loader :: (ToJSON a, FromJSON a) => FilePath -> IO (Maybe a)
loader fname = return . decode =<< BS.readFile fname

loadReservationDB, loadGuidesDB, loadDriversDB, loadCarsDB, loadUsersDB :: (FromJSON a, ToJSON a, Eq a) => IO (Maybe [a])
loadUsersDB   = loader usersDB
loadCarsDB    = loader carsDB
loadDriversDB = loader driversDB
loadGuidesDB  = loader guidesDB
loadReservationDB = loader reservationsDB

-- Data base writers for users, drivers, etc...

db_writer :: (FromJSON a, ToJSON a) => FilePath -> a -> IO ()
db_writer location object =
  BS.writeFile location . encode . construct =<< loader location
  where
    construct = maybe [object] (object:)

db_write_many :: (FromJSON a, ToJSON a) => FilePath -> [a] -> IO ()
db_write_many location = BS.writeFile location . encode

ask :: String -> IO (String)
ask message = do
  putStrLn message
  getLine
lookupForGuidesByLanguages :: [Language] -> IO [Guide]
lookupForGuidesByLanguages langs =  extractCandidates <$> loadGuidesDB
  where
    extractCandidates = maybe [] (filter (\(Guide _ l _) -> l `elem2` langs))
    elem2 :: [Language] -> [Language] -> Bool
    elem2 [] [] = False
    elem2 xs [] = False
    elem2 [] xs = False
    elem2 (x:zs) xs = (x `elem` xs) ||  (elem2 zs xs)

pivot = maybe [] id
showDriversByPredicate :: (Driver -> Bool) -> IO ()
showDriversByPredicate predicate =   showEachDriver
                                   . filter predicate
                                   . pivot
                                   =<< loadDriversDB
  where
    extractStatus :: Maybe Car -> String
    extractStatus (Just (Car c _ _ _ _ _ _)) = " De capacite " ++ show c
    extractStatus Nothing                    = " Inconnue "
    showEachDriver :: [Driver] -> IO ()
    showEachDriver =
      mapM_ (\x ->
        putStrLn ("| name : "                ++  dname x                      ++
                 " | affecte a la voiture :" ++  extractStatus (affectedTo x) ++
                 " | Date  :"        ++  (at1 x)) >>
      showLine)

lookupForCar :: Int -> IO (Maybe Car)
lookupForCar capacity = sHead
                      . filter (\(Car c _ _ _ _ _ _) -> c == capacity)
                      . pivot
                      <$> loadObj

showDrivers = showDriversByPredicate   (\x -> x == x)
lookupForDriversByName :: [Name] -> IO [Driver]
lookupForDriversByName names = return . select =<< loadDriversDB
  where
    select = maybe [] $ filter (\(Driver name _ _) -> name `elem` names)
showLine :: IO ()
showLine = putStrLn $ replicate 162 '_'

showCarsList :: IO ()
showCarsList = do
  putStrLn "Liste des voitures: "
  showLine
  showEachcar =<< loadCarsDB
  where
    status :: Bool -> String
    status True  = "Oui"
    status False = "Non"

    dateStatus :: String -> String
    dateStatus "" = "INDETERMINE"
    dateStatus other = other

    showEachcar :: Maybe [Car] -> IO ()
    showEachcar  = maybe (putStrLn "") $
      mapM_ (\x ->
        putStrLn ("| Capacite : "          ++  show (capacity x)     ++
                 " | Distance parcourue :" ++  show (elapsed_dist x) ++
                 " | Date de Depart:"      ++  dateStatus (d_date x) ++
                 " | Heure de depart:"     ++  show (d_hour x)       ++
                 " | Date d'arrive :"      ++  dateStatus (r_date x) ++
                 " | Heure d'arrive:"      ++  show (r_hour x)       ++
                 " | EST RESERVE :"        ++  status (reserved x)) >>
      showLine)

mkUser :: IO User
mkUser = do
  name     <- ask "Entrer le nom de l'utilisateur : "
  password <- ask . concat $ ["Entrer le password de ", name,  " : "]
  return $ User name password

mkDriver :: IO (Driver)
mkDriver = do
  dname  <- ask "Entrer le nom du chauffeur: "
  answer <- ask "Est-il affecte a une voiture? (Y/N) "
  date <- ask "Entrer la date:"
  case answer of
    "N" -> return $ Driver dname Nothing date
    "Y" -> do
        showCarsList
        vname <- ask "\nEntrer la capacite de la voiture: "
        resp  <- lookupFor (read vname) loadCarsDB
        case resp of
          c@(Just _)  -> return $ Driver dname c date
          Nothing -> do
            res <- ask ("La Voiture de capacite " ++ show vname ++ " est inexistante, ressayer une nouvelle fois? (Y/N) ")
            if res == "Y" then
              mkDriver
            else return sampleDriver
    _  -> error "Choix invalide"

sHead :: [a] -> Maybe a
sHead []     = Nothing
sHead (x:xs) = Just x

lookupFor :: Int -> IO (Maybe [Car]) -> IO (Maybe Car)
lookupFor name dbFunc = do
  values <- dbFunc
  case values of
    Just cars -> return . sHead $ filter (\(Car a _ _ _ _ _ _) -> a == name) cars
    Nothing   -> return Nothing

addUser, addGuide, addCar, addDriver :: IO ()
addDriver =  db_writer driversDB =<< mkDriver
addCar    =  db_writer carsDB    =<< mkCar
addGuide  =  db_writer guidesDB  =<< mkGuide
addUser   =  db_writer usersDB   =<< mkUser
addReservation = db_writer reservationsDB =<< mkReservation

addNCars :: Int -> IO ()
addNCars 0 = return ()
addNObj n func = sequence_ $ replicate n func

mkCar :: IO Car
mkCar = do
  capacity <- ask "La capacite de la voiture: "
  return $ Car (read capacity) 0 0 0 "" "" False

assignCarDist :: Int -> IO (Car) -> IO (Car)
assignCarDist dist = (<$>) (\(Car a _ c d e f g) -> Car a dist c d e f g )

assignCarCapacity :: Int -> IO (Car) -> IO (Car)
assignCarCapacity capacity = (<$>) (\(Car _ b c d e f g) -> Car capacity b c d e f g )

assignReservationDateDeDepart :: (Date, Hour) -> IO (Reservation) -> IO (Reservation)
assignReservationDateDeDepart cal = (<$>) (\(Reservation _ b c d e f g) -> Reservation cal b c d e f g)

assignReservationDateDeDarrive :: (Date, Hour) -> IO (Reservation) -> IO (Reservation)
assignReservationDateDeDarrive cal = (<$>) (\(Reservation a _ c d e f g) -> Reservation a cal c d e f g)

assignReservationDestination :: IO Reservation -> IO Reservation
assignReservationDestination res = do
  dest <- ask "Enter une destination: "
  time <- ask "Entrer le temps d'attente: "
  (\(Reservation a b c d e f g) ->
     Reservation a b ((read time :: Int, dest):c) d e f g) <$> res


assignReservationGuides :: IO Reservation -> IO Reservation
assignReservationGuides rst = undefined


lookupForCarByCapacity :: Int -> IO (Maybe Car)
lookupForCarByCapacity n = do
  Just cars <- loadCarsDB
  let items = filter (\(Car a _ _ _ _ _ _) -> a == n) cars
  case items of
    []     -> return Nothing
    (x:xs) -> return (Just x)

assignDriverCar :: IO Driver -> IO Driver
assignDriverCar driver = do
  content <- driver
  showCarsList
  capacity <- ask "Entrer la capacite de la voiture: "
  mcar <- lookupForCarByCapacity (read capacity)
  case (content, mcar) of
    (_           , Nothing) -> error "Une voiture de cette capacite n'est pas disponible."
    ((Driver n _ at), mcar) -> return $ Driver n mcar at

mkGuide :: IO (Guide)
mkGuide = do
  gname <- ask "Le nom du guide: "
  putStrLn "Entrer les noms des langues une par une et terminez par F"
  languages <- collect_languages []
  return $ Guide gname languages False

collect_languages :: [String] -> IO ([String])
collect_languages current = do
  gname <- ask "Entrer le nom d'une langue: "
  case gname of
    "F" -> return current
    _   -> collect_languages (gname:current)

collect_destinations :: [(Int, String)] -> IO ([(Int, String)])
collect_destinations current = do
  gname <- ask "Entrer le nom de la destination: "
  dest  <- ask "Entrer la distance a la destination: "
  case gname of
    "F" -> return current
    _   -> collect_destinations ((read dest, gname):current)

collect_drivers :: [Name] -> IO ([Name])
collect_drivers current = do
  name <- ask "Entrer le nom du chauffeur: "
  case name of
    "F" -> return current
    _   -> collect_drivers (name:current)


parseDate :: String -> (Int, Int, Int)
parseDate [] = error "Date invalide."
parseDate (d1:d2:'/':m1:m2:'/':year) = (read (d1:d2:[]), read (m1:m2:[]), read year)
parseDate _ = error "Date invalide."

belongsToDateRange :: [(Int, Int, Int)]
                   -- -> (Int, Int, Int)
                   -- -> (Int, Int, Int)
                   -> Bool
belongsToDateRange ((d, m, y):(d1, m1, y1):(d2, m2, y2):[]) = (y1 <= y && y <= y2) &&
                                                              (m1 <= m && m <= m2) &&
                                                              (d1 <= d && d <= d2)
belongsToDateRange _ = False

mkReservation :: IO (Reservation)
mkReservation = do
  cdate  <- ask "Entrer la date actuelle:  "
  dateD  <- ask "Entrer la date de depart: "
  heurD  <- ask "Entrer l'heure de depart: "
  dateR  <- ask "Entrer la date d'arrive:  "
  heurR  <- ask "Entrer l'heure de d'arrive: "
  answer <- ask "Voulez-vous des guides? (Y/N): "
  langs <-  case answer of
        "Y" -> do
          putStrLn "Entrer les langues qui vous interresse et terminez pas F: "
          collect_languages []
        _  -> return []
  guides <- lookupForGuidesByLanguages langs
  unless (not (null guides) || null langs) $
    putStrLn "AUCUN GUIDE NE PARLE LES LANGUES CHOISIES!"

  putStrLn "Entrer les noms des destinations une par une et terminez par F"
  destItems <- collect_destinations []

  showDrivers
  putStrLn "Choisissez les chauffeurs qui vous interresse: et terminez par F"
  dnames  <- collect_drivers []
  drivers <- lookupForDriversByName dnames

  putStrLn "Affectez maintenant des voitures a ces chauffeurs"
  putStrLn "Pour cela, choisir la capacite de la voiture parmis ceux-ci: "
  showCarsList
  items <- mapM attributeCar dnames
  let driverWithCars = map (\((name, car), driver) -> (driver, car)) $ zip items drivers
  return $ Reservation (dateD, read heurD)     (dateR, read heurR)
                       destItems (Just guides) cdate
                       driverWithCars ""

filterByDate :: (LoaderInterface a, Eq a, ToJSON a, FromJSON a)  -- Quite obvious type constrains!
             =>  (Date, Date)
             ->  IO (Maybe [a])                                  -- The date
             ->  IO [a]
filterByDate (d1, d2) loadr = filter (predicate . theDate) . pivot <$> loadr
  where
    predicate :: Date -> Bool
    predicate d3 = belongsToDateRange $ map parseDate [d1, d2, d3]

attributeCar :: Name -> IO (Name, Car)
attributeCar name = do
  putStrLn ("Attribuer une voiture a " ++ name)
  ansi <- ask "Capacite: "
  mcar <- lookupFor (read ansi) loadCarsDB
  case mcar of
    Nothing    -> error "Voiture inconnue"
    (Just car) -> return (name, car)

sampleDriver :: Driver
sampleDriver = Driver "Franck" Nothing ""

-- Remove an object from the data base
delete :: (LoaderInterface a, Eq a, ToJSON a, FromJSON a) -- Quite obvious type constrains!
       => FilePath                                        -- Location of the data base in the file system
       -> a                                               -- Can be a Car, a Driver ...
--        -> IO (Maybe [a])                               -- Two layer of the monad instances for loading the data base
       -> IO ()                                           -- I/O operation without a return value, just mutate a file on the disk.
delete location obj =
  db_write_many location . filter (/= obj) . pivot =<< loadObj

--- Simple specializations of the previous function for each object

deleteUser :: User -> IO ()
deleteUser user = delete  usersDB user

deleteGuide :: Guide -> IO ()
deleteGuide guide = delete guidesDB guide

deleteCar :: Car -> IO ()
deleteCar car = delete carsDB car

deleteReservation :: Reservation -> IO ()
deleteReservation reservation = delete reservationsDB reservation

deleteDriver :: Driver -> IO ()
deleteDriver driver = delete driversDB driver
