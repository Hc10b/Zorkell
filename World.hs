module World where

import Action
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe


type Conversations = Map String String
type Interactions  = Map Action String
type LocationID    = String
type LocationMap   = Map LocationID Location
type ObjectID      = String
type ObjectMap     = Map ObjectID Object
type ObjLocMap     = Map LocationID ObjectID


-- | Check if a string matches an object and process an action on the object, return the World State, and a string to tell the user what happened.
-- 
class Actionable f where
    actOn :: f -> Action -> State World [String]
    match :: String -> f -> Bool


-- | World data type and constructor. Describes the world with the current location, as well as mappings of all locations and objects in the world, and provides a mapping of objects and characters to locations in the mapping. Also provides a smart data constructor.
-- 
data World = World { currentLocation :: Location
                   , worldLocations :: LocationMap
                   , worldObjects :: ObjectMap
                   , objectLocations :: ObjLocMap }
                   
world :: Location -> LocationMap -> ObjectMap -> ObjLocMap -> World
world = World

-- | Create the location mapping for the world from a list of locations. Maps the location name to the location ID.
-- 
mapFromLocations :: [Location] -> LocationMap
mapFromLocations locs = Map.fromList $ map ((,) <$> locationName <*> id) locs

-- | Looks in the world map, and provides a location by its name.
-- 
getLocationByName :: World -> LocationID -> Location
getLocationByName wrld loc = fromMaybe (error "That is not a valid roon name.") $ Map.lookup loc $ worldLocations wrld

-- | Create the object mapping for the world from a list of objects. Maps the object name to the object ID.
--
mapFromObjects :: [Object] -> ObjectMap 
mapFromObjects objs = Map.fromList $ map ((,) <$> objectName <*> id) objs

-- | Looks in the world map, and provides an object by its name.
-- 
getObjectByName :: World -> ObjectID -> Object
getObjectByName wrld obj = fromMaybe (error "That is not a valid object name.") $ Map.lookup obj $ worldObjects wrld

-- | Create the object location mapping for the world from a list of object name and location name pairs. Maps the object name to the location name.
-- 
mapFromLocObj :: [(ObjectID, LocationID)] -> ObjLocMap -> ObjLocMap
mapFromLocObj []               m = m
mapFromLocObj ((obj, loc):ols) m = mapFromLocObj ols (Map.insert obj loc m)

-- | Looks in the world map, and provides a list of objects in the current location.
-- 
getLocByObj :: World -> ObjectID -> LocationID
getLocByObj wrld obj = fromMaybe (error "There are no objects in here.") $ Map.lookup obj $ objectLocations wrld

-- | Is the object requested by the user at their location?
-- 
isAtLoc :: ObjectID -> LocationID -> World -> Bool
isAtLoc obj loc wrld
    | obj `elem` (Map.keys (Map.filter (== loc) (objectLocations wrld))) = True
    | otherwise                                                          = False
    

-- | Location data type and constructor. Exits are included in this data type, because they are immutable (in the sense that they cannot be moved from one location to another). Also includes a smart data constructor, as well as eq, and ord instances.
-- 
data Location = Location { locationName :: String
                         , locationDescription :: String }
    
location :: String -> String -> Location
location = Location

instance Eq Location where
    (==) l1 l2 = locationName l1 == locationName l2
    
instance Ord Location where
    l1 `compare` l2 = locationName l1 `compare` locationName l2
    
    
-- | Object data type and constructor. Describes the objects with name/description and actions. Exits are also modeled as objects in this refactored version for increased generalization. Also includes a smart constructor, as well as show, eq, and actionable instances.
-- 
data Object = Object { objectName :: String
                     , objectAlias :: [String]
                     , objectDescription :: String
                     , objectReactions :: Action -> State World [String] }
                                                
object :: String -> [String] -> String -> (Action -> State World [String]) -> Object
object = Object

exit :: String -> [String] -> String -> (Action -> State World [String]) -> Object
exit = Object

-- | Find the interactions available for the selected object, and return to the AdventureEngine to see if an action can be performed on it.
-- 
findInteractions :: ObjectID -> LocationID -> World -> Maybe (Action -> State World [String])
findInteractions obj loc wrld = case isAtLoc obj loc wrld of
    True  -> Just (objectReactions (getObjectByName wrld obj))
    False -> Nothing
                             
instance Show Object where
    show = show . objectName
    
instance Eq Object where
    (==) o1 o2 = objectName o1 == objectName o2
    
instance Actionable Object where
    actOn = objectReactions
    match strng = (strng `elem`) . ((:) <$> objectName <*> objectAlias)
    

-- | Character data type and constructor. I haven't gotten to the maze portion of the map yet, so I don't have this implemented anywhere. Also includes a smart constructor, as well as an actionable instance.
-- 
data Character = Character { characterName :: String
                           , characterAlias :: [String]
                           , characterTopics :: Conversations
                           , characterReactions :: Interactions }
    deriving (Eq,Show)

character :: String -> [String] -> Conversations -> Interactions -> Character
character = Character

instance Actionable Character where
    actOn chrctr action = error "This hasn't been implemented yet."
    match strng         = (strng `elem`) . ((:) <$> characterName <*> characterAlias)
        
    
-- | Takes in a room and the movement command, and updates the current location in the world state.
-- 
basicMove :: Location -> Action -> State World [String]
basicMove loc Go = do get >>= \wrld -> put wrld{ currentLocation = loc } >>= \_ -> singleAnswer $ (map (const '*') (locationName loc)) ++ "\n" ++ (map toUpper (locationName loc)) ++ "\n" ++ (map (const '*') (locationName loc)) ++ "\n" ++ (locationDescription loc)
basicMove _   _  = singleAnswer "You can't go that direction."


-- | Return the result of performing an action.
-- 
singleAnswer :: String -> State World [String]
singleAnswer = return . (:[])