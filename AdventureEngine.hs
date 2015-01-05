module AdventureEngine where

import Action
import Control.Applicative
import Control.Monad.State
import Data.Char
import qualified Data.Map as Map
import Data.Map(Map)
import GameData
import Parser
import System.IO
import World

loadGame = do
    printStrs . displayLocation $ currentLocation aWorld
    promptLoop aWorld
    
    
-- | Display a prompt, get an input from the player, call a function to see if an action can be completed, and return the state world if it can.
-- 
promptLoop :: World -> IO ()
promptLoop wrld = do
    input <- getAction
    let action = processCommand input commands
    either putStrLn (runAction wrld) $ quitOrContinue action
    
    
-- | Execute an action in the world.
-- 
runAction :: World -> State World [String] -> IO ()
runAction wrld st = do
    printStrs $ fst reaction
    promptLoop $ snd reaction
    where
        reaction = runState st wrld
        

-- | Ask the user for an action.
-- 
getAction :: IO String
getAction = do
    putStr "> "
    hFlush stdout
    getLine
    
    
-- | Check if the proposed action by the user is to quit, or do something else.
-- 
quitOrContinue :: PlayerAction -> Either String (State World [String])
quitOrContinue (SimpleAction Quit) = Left "Leaving the world of Zorkell."
quitOrContinue action              = Right $ continue action


-- | Given the world and an action, proceed with the action and return the State World [String].
-- 
continue :: PlayerAction -> State World [String]
continue (SimpleAction Zilch)     = singleAnswer "You'll have to tell me how to do that."
continue (SimpleAction Examine)   = state $ (,) <$> displayLocation . currentLocation <*> id
continue (SimpleAction Inventory) = singleAnswer (displayInventory inventory)
continue (Interaction action obj) = do
    wrld <- get
    case findInteractions obj (locationName (currentLocation wrld)) wrld of
        Nothing -> singleAnswer $ "There is no " ++ obj ++ " in the room!"
        Just f  -> f action
continue _                        = singleAnswer "I don't understand what you are trying to do."


-- | Let the user see the response to the command given.
-- 
printStrs = mapM putStrLn


-- | Display the location description to the player.
-- 
displayLocation :: Location -> [String]
displayLocation (Location name desc) = [stars] ++ displayName ++ [stars] ++ [desc]
    where
        stars = map (const '*') name
        displayName = [map toUpper name]
        

-- | Display the users current inventory when requested.
-- 
displayInventory :: Location -> String
displayInventory (Location name desc) = displayName ++ "\n" ++ desc
    where
        displayName = map toUpper name