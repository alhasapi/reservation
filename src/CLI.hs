{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI (userCLI) where

import DB
import DataTypes
import Data.List
import Control.Monad (unless)
import Data.Yaml (ToJSON, FromJSON)
import System.Process (createProcess, shell)

authenticateUser :: UserName  -- self-evident description
                 -> Password  -- idem
                 -> IO Bool   -- The authentification response is wrapped in the IO monad.
authenticateUser userName passWord = maybe False (User userName passWord `elem`) <$> loadUsersDB

-- Show the list of each guide of by its regular attributes
showGuidesList :: IO ()
showGuidesList = showEachGuide =<< loadGuidesDB
  where
    showEachGuide :: Maybe [Guide] -> IO ()
    showEachGuide  =
      maybe (return ()) $
      mapM_ (\x ->
       putStrLn ("Nom du guide : "            ++ gname x                     ++
                 " | Les Langues du guides: " ++ (intercalate ", " . languages $ x) ++
                 " | Status: "                ++ (checkStatus . busy $ x)
                ) >> showLine
         )
    checkStatus True = "Occupe"
    checkStatus False = "Libre"

-- Show the list of each user of by its regular attributes
showUserList :: IO ()
showUserList = showEachUser =<< loadUsersDB
  where
    showEachUser :: Maybe [User] -> IO ()
    showEachUser = maybe (return ()) $
      mapM_ (\x -> putStrLn ("Nom : "  ++ name x ++ " | Mot des passe: CACHE") >> showLine)
    extractLg [] = ""
    extractLg xs = concat $ intersperse ", "

    checkStatus :: Bool -> String
    checkStatus True = "Occupe"
    checkStatus False = "Libre"

-- Generic filter by name function
generiqLookup :: (LoaderInterface a, ExEq a, FromJSON a, ToJSON a)  -- We must be able to load the data base
              => Name                                               -- Quite self-evident
              -> IO (Maybe a)                                       -- The result is wrapped twice: by the IO and the Maybe monad
generiqLookup name = sHead
                . filter (=<>= name)
                . pivot
                <$> loadObj

-- Show the list of each reservation of by its regular attributes
showReservationList :: IO ()
showReservationList = showEachReservation =<< loadReservationDB
  where
    showEachReservation :: Maybe [Reservation] -> IO ()
    showEachReservation = maybe (return ()) $
      mapM_ (\x ->
               putStrLn ("Date de Reservation: "      ++ registedAt x                                       ++ "\n" ++
                         " | Desitations: "    ++ (intercalate ", " . map snd . destinations $ x)           ++ "\n" ++
                         " | Avec de(s) guide(s): "   ++ (checkGuide . withGuides $ x)       ++ "\n"        ++
                         " | A reserve:      "    ++ (show . length . reserverdCars $ x)     ++ " Voitures" ++ "\n" ++
                         " | Date de depart: "    ++ (fst  . dWhenAndWhere $ x)              ++ "\n" ++
                         " | Heure de depart:"    ++ (show . snd  . dWhenAndWhere $ x)       ++ "\n" ++
                         " | Date d'arrive:  "    ++ (fst  . rWhenAndWhere $ x)              ++ "\n" ++
                         " | Heure d'arrive: "    ++ (show . snd  . rWhenAndWhere $ x)       ++ "\n" ++
                         " | Enregistre a:  "    ++ at x) >> showLine)

    checkGuide :: Maybe [Guide] -> String
    checkGuide Nothing   = "Aucun Guide attribue"
    checkGuide (Just []) = "Aucun Guide attribue"
    checkGuide (Just x)  = intercalate ", " $ map (\(Guide n _ _) -> n) x

removeObj remover shower errorMsg = do
  shower
  v <- ask "Faite votre choix: "
  mobj <- generiqLookup v
  case mobj of
    Just o  -> remover o
    Nothing -> putStrLn errorMsg

removeCar errorMsg = do
  showCarsList
  v <- ask "Faite votre choix: "
  mobj <- lookupForCar (read v)
  case mobj of
    Just o  -> deleteCar o
    Nothing -> putStrLn errorMsg

schema msg shower adder deleter = do
  putStrLn msg
  resp <- ask "Votre Choix: "
  case (read resp :: Int) of
    1 -> shower
    2 -> adder
    3 -> deleter
  _ <- createProcess $ shell "clear"
  return ()


adminMsg, carsMsg, guideMsg, userMsg :: String
carsMsg   = "Entrer 1 Pour la liste des voitures    \n       2 Pour ajouter une voitures   \n       3 Pour supprimer une voiture "
guideMsg  = "Entrer 1 Pour la liste des guides      \n       2 Pour ajouter un guide       \n       3 Pour supprimer un guide "
driverMsg = "Entrer 1 Pour la liste des chauffeurs  \n       2 Pour ajouter un chauffeur   \n       3 Pour supprimer un chauffeur "
userMsg   = "Entrer 1 Pour la liste des utilisateurs\n       2 Pour ajouter un utilisateur \n       3 Pour supprimer un utilisateur "
adminMsg  = "Entrer 1 pour la gestion des Voitures  \n       2 pour la gestion des guides  \n       3 pour la gestion des Utilisateurs\n       4 pour la gestion des chauffeurs\n       5 pour la gestion des reservations "
reservationMsg = "Entrer 1 Pour la liste des reservations    \n       2 Pour ajouter une ajouter une reservation   \n       3 Pour supprimer une reservation\n       4 Pour modifier une reservation "
driverManagement, carsManagement, guidesManagement, userManagement :: IO ()
carsManagement   = schema carsMsg   showCarsList   addCar    (removeCar "Voiture inconnue")
driverManagement = schema driverMsg showDrivers    addDriver (removeObj  deleteDriver showDrivers    "Chauffeur inconnue")
guidesManagement = schema guideMsg  showGuidesList addGuide  (removeObj  deleteGuide  showGuidesList "Guide inconnue")
userManagement   = schema userMsg   showUserList   addUser   (removeObj  deleteUser   showUserList   "Utilisateur inconnue")
reservationManagement :: IO ()
reservationManagement = do
  putStrLn reservationMsg
  resp <- ask "Votre Choix: "
  case (read resp :: Int) of
    1 -> showReservationList
    2 -> addReservation
    3 -> removeObj deleteReservation showReservationList "Reservation inconnue."
  createProcess $ shell "clear"
  return ()

clearScreen = createProcess $ shell "clear"

userCLI, adminCLI :: IO ()
adminCLI = do
  putStrLn "<----------------> Interface d'administration <----------------------->"
  putStrLn adminMsg
  resp <- ask "Votre Choix: "
  case (read resp :: Int) of
    1 -> carsManagement
    2 -> guidesManagement
    3 -> userManagement
    4 -> driverManagement
    5 -> reservationManagement
  clearScreen >> adminCLI

stepAuth :: IO Bool
stepAuth = do
 userName <- ask "Nom d'utilisateur: "
 password <- ask "Mot de passe: "
 isValid  <- authenticateUser userName password
 unless isValid $
  putStrLn "Nom d'utilisateur ou mot de passe invalide"
 return isValid

userCLI = do
  _ <- createProcess $ shell "clear"
  stepAuth >>= \resp -> if resp then adminCLI else userCLI
