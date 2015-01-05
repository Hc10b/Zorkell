module Parser where

import Action
import Data.Char
import Data.List


-- | Define articles as a sentence part. Used in identifying objecst in the user provided commands.
-- 
articles = ["the", "a", "an"]


-- | Process the command input from the user. Builds an action from the user provided string, and a list of the commands (from the Action module).
-- 
processCommand :: String -> [Command] -> PlayerAction
processCommand strng commands = buildAction command objects
    where
        command = findCommand commands statement
        objects = findObjects (tail statement) command
        statement = words . map (toLower) $ strng
        -- | Helper function for processCommand. Tries to identify if there is a command in the string provided by the user, and if so, return the appropriate command action from Action module.
        --
        buildAction :: Maybe Command -> [String] -> PlayerAction
        buildAction Nothing    _             = SimpleAction Zilch
        buildAction (Just cmd) []            = SimpleAction (getCommandAction cmd)
        buildAction (Just cmd) [obj]         = Interaction (getCommandAction cmd) obj
        buildAction (Just cmd) (obj:objs:[]) = Complex (getCommandAction cmd) obj objs
        
        
-- | Given a list of commands, try to find one that can be acted on.
-- 
findCommand :: [Command] -> [String] -> Maybe Command
findCommand []         strngs      = Nothing
findCommand (cmd:cmds) strngs
    | commandInSentence cmd strngs = Just cmd
    | otherwise                    = findCommand cmds strngs
    where
        commandInSentence :: Command -> [String] -> Bool
        commandInSentence (Transitive _ form prep _)      strngs = form `elem` strngs && prep `contains` strngs
        commandInSentence (Phrasal    _ form phra prep _) strngs = form `elem` strngs && phra `elem` strngs && prep `contains` strngs
        
contains :: [String] -> [String] -> Bool
contains [] _  = True
contains xs ys = intersect xs ys /= []


-- | Given a sentence without the command, see if there are recognizable objects in the sentence. There could potentially be zero, one, or two objects.
-- 
analyzeObjects :: [String] -> [String] -> [String] -> [String]
analyzeObjects sentence fstWds sndWds = matchObject sentence (findPositionOf fstWds sentence) (findPositionOf sndWds sentence)
    where
        -- | Helper function for analyzeObjects. After removing the articles from the string that remains after parsing the command, try to match it to an object.
        --
        matchObject :: [String] -> Maybe Int -> Maybe Int -> [String]
        matchObject [] _         _           = []
        matchObject s  Nothing   Nothing     = [concRemArticles s]
        matchObject s  Nothing   (Just obj)  = [concRemArticles . take obj $ s] ++ [concRemArticles . drop obj $ s]
        matchObject s (Just 0)   Nothing     = [concRemArticles . tail $ s]
        matchObject s (Just obj) Nothing     = [concRemArticles . take obj $ s]
        matchObject s (Just 0)   (Just obj)  = [concRemArticles . take (obj - 1) . tail $ s] ++ [concRemArticles . drop (obj + 1) $ s]
        matchObject s (Just obj) (Just obj') = [concRemArticles . init . take (obj - 1) $ s] ++ [concRemArticles . drop (obj' + 1) $ s]
        -- | Helper function for analyzeObject. Return the string list location of user input for the preposition (in the case of a Transitive statement), or the phrasal and preposition (in the case of a Phrasal statement).
        --
        findPositionOf :: [String] -> [String] -> Maybe Int
        findPositionOf prep sentence = findIndex (`elem` prep) sentence
        -- | Helper function for matchObject. Removes the articles from the string list that remains after the command has been parsed from it.
        --
        concRemArticles :: [String] -> String
        concRemArticles = unwords . (flip (\\) articles)
        

-- | Find the objects in a processCommand and return the list of strings with those objects.
-- 
findObjects :: [String] -> Maybe Command -> [String]
findObjects _        Nothing                                = []
findObjects sentence (Just (Transitive _ form prep comp))   = analyzeObjects sentence prep comp
findObjects sentence (Just (Phrasal _ form phra prep comp)) = analyzeObjects sentence (phra:prep) comp