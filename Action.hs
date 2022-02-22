module Zorkell.Action where


-- | Sentance constructors to receive actions. Transitive verb structure makes the actions fairly simple to sort from the rest of the sentance, but Phrasal verb structures require a form and phrasal part that necessarily change the meaning.
-- 
data Command = Transitive { actionType :: Action, form :: String, preposition :: [String], complement :: [String] }
             | Phrasal    { actionType :: Action, form :: String, phrasal :: String, preposition :: [String], complement :: [String] }
    deriving (Eq,Show)


-- | Player action data type and constructors. Uses the default eq and show instances.
-- 
data PlayerAction = SimpleAction Action
                  | Interaction Action String
                  | Complex Action String String
    deriving (Eq,Show)


-- | Action data type and contructors. Uses the default eq, ord, and show instances.
-- 
data Action = Close
            | Consume
            | Drop
            | Examine
            | Inventory
            | Go
            | Kill
            | Move
            | Open
            | Put
            | Quit
            | Search
            | Take
            | Talk
            | TurnOff
            | TurnOn
            | Zilch
    deriving (Eq,Ord,Show)
    
    
-- | Commands used in completing/processing action phrases.
-- 
close   = Transitive Close "close" [] []
shut    = Transitive Close "shut" [] []
eat     = Transitive Consume "eat" [] []
drink   = Transitive Consume "drink" [] []
dropIt  = Transitive Drop "drop" [] []
examine = Transitive Examine "examine" [] []
look    = Transitive Examine "look" ["at", "for"] ["with"]
readIt  = Transitive Examine "read" [] []
go      = Transitive Go "go" [] []
getInv  = Transitive Inventory "inventory" [] []
hit     = Transitive Kill "hit" [] ["with"]
kill    = Transitive Kill "kill" [] ["with"]
move    = Transitive Move "move" [] []
open    = Transitive Open "open" [] []
putIt   = Transitive Put "put" [] []
quit    = Transitive Quit "quit" [] []
takeIt  = Transitive Take "take" [] []
getIt   = Transitive Take "get" [] []
pick    = Phrasal Take "pick" "up" ["with"] []
speak   = Transitive Talk "speak" ["with", "to"] ["about"]
talk    = Transitive Talk "talk" ["with", "to"] ["about"]
ask     = Transitive Talk "ask" [] ["about"]
turnOff = Phrasal TurnOff "turn" "off" [] []
turnOn  = Phrasal TurnOn "turn" "on" [] []

-- | Command definitions for use in the Adventure Engine module.
-- 
commands = [close, shut, eat, drink, dropIt, examine, look, readIt, go, getInv, kill, move, open, putIt, quit, takeIt, getIt, pick, speak, talk, ask]


-- | Parse the command from the user provided input.
-- 
getCommandAction :: Command -> Action
getCommandAction (Transitive cmd _ _ _)   = cmd
getCommandAction (Phrasal    cmd _ _ _ _) = cmd


-- | Transform the command into an action if possible, otherwise return the default response.
-- 
commandIntoAction :: Maybe Command -> Maybe String -> Maybe String -> PlayerAction
commandIntoAction Nothing    _          _           = SimpleAction Zilch
commandIntoAction (Just cmd) Nothing    _           = SimpleAction (getCommandAction cmd)
commandIntoAction (Just cmd) (Just obj) Nothing     = Interaction (getCommandAction cmd) obj
commandIntoAction (Just cmd) (Just obj) (Just obj') = Complex (getCommandAction cmd) obj obj'