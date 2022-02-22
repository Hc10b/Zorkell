module Zorkell.GameData where

import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe
import Zorkell.Action
import Zorkell.World


-- | Starting World State.
-- 
aWorld = world
    westOfHouse
    (mapFromLocations [westOfHouse
                      , northOfHouse
                      , behindHouse
                      , southOfHouse
                      , forest1
                      , clearing1
                      , forest2
                      , clearing2
                      , forest3
                      , forestPath
                      , upTree
                      , forest4
                      , canyonView
                      , rockyLedge
                      , canyonBottom
                      , endOfRainbow
                      , kitchen
                      , atticDark
                      , attic
                      , livingRoom])
    (mapFromObjects [fromWestOfHouseToNorthOfHouse
                    , fromWestOfHouseToSouthOfHouse
                    , fromWestOfHouseToForest1
                    , fromNorthOfHouseToForestPath
                    , fromNorthOfHouseToBehindHouse
                    , fromNorthOfHouseToWestOfHouse
                    , fromBehindHouseToNorthOfHouse
                    , fromBehindHouseToClearing2
                    , fromBehindHouseToSouthOfHouse
                    , fromSouthOfHouseToBehindHouse
                    , fromSouthOfHouseToForest3
                    , fromSouthOfHouseToWestOfHouse
                    , fromForest1ToClearing1
                    , fromForest1ToForestPath
                    , fromForest1ToForest3
                    , fromClearing1ToForest2
                    , fromClearing1ToForestPath
                    , fromClearing1ToForest1
                    , fromForestPathToClearing1
                    , fromForestPathUpTree
                    , fromForestPathToForest2
                    , fromForestPathToNorthOfHouse
                    , fromForestPathToForest1
                    , fromUpTreeToForestPath
                    , fromForest2ToForest4
                    , fromForest2ToClearing2
                    , fromForest2ToForestPath
                    , fromForest4ToForest2N
                    , fromForest4ToForest2E
                    , fromForest4ToForest2S
                    , fromClearing2ToForest2
                    , fromClearing2ToCanyonView
                    , fromClearing2ToForest3
                    , fromClearing2ToBehindHouse
                    , fromCanyonViewToRockyLedge
                    , fromCanyonViewToForest3
                    , fromCanyonViewToClearing2
                    , fromRockyLedgeToCanyonView
                    , fromRockyLedgeToCanyonBottom
                    , fromCanyonBottomToRockyLedge
                    , fromCanyonBottomToRainbow
                    , fromRainbowToCanyonBottom
                    , fromForest3ToClearing2
                    , fromForest3ToForest1
                    , fromForest3ToSouthOfHouse
                    , fromKitchenToAtticDark
                    , fromKitchenToBehindHouse
                    , fromKitchenToLivingRoom
                    , fromAtticToKitchen
                    , fromLivingRoomToKitchen
                    , fromBehindHouseToKitchen
                    , fromKitchenToAttic
                    , mailbox
                    , leaflet
                    , jeweledEgg
                    , leaves
                    , brownSack
                    , lunch
                    , garlic
                    , bottle
                    , rope
                    , nastyKnife
                    , trophyCase
                    , brassLamp
                    , sword
                    , rug
                    , window
                    , quantityOfWater])
    (mapFromLocObj [("north from west of house", "west of house")
                   , ("south from west of house", "west of house")
                   , ("west from west of house", "west of house")
                   , ("mailbox", "west of house")
                   , ("north from north of house", "north of house")
                   , ("east from north of house", "north of house")
                   , ("west from north of house", "north of house")
                   , ("north from behind house", "behind house")
                   , ("east from behind house", "behind house")
                   , ("south from behind house", "behind house")
                   , ("a small window which is slightly ajar", "behind house")
                   , ("east from south of house", "south of house")
                   , ("south from south of house", "south of house")
                   , ("west from south of house", "south of house")
                   , ("north from forest 1", "forest 1")
                   , ("east from forest 1", "forest 1")
                   , ("south from forest 1", "forest 1")
                   , ("east from clearing 1", "clearing 1")
                   , ("south from clearing 1", "clearing 1")
                   , ("west from clearing 1", "clearing 1")
                   , ("leaves", "clearing 1")
                   , ("north from forest path", "forest path")
                   , ("up from forest path", "forest path")
                   , ("east from forest path", "forest path")
                   , ("south from forest path", "forest path")
                   , ("west from forest path", "forest path")
                   , ("down from up tree", "up tree")
                   , ("jeweledEgg", "up tree")
                   , ("east from forest 2", "forest 2")
                   , ("south from forest 2", "forest 2")
                   , ("west from forest 2", "forest 2")
                   , ("north from forest 4", "forest 4")
                   , ("east from forest 4", "forest 4")
                   , ("south from forest 4", "forest 4")
                   , ("north from clearing 2", "clearing 2")
                   , ("east from clearing 2", "clearing 2")
                   , ("south from clearing 2", "clearing 2")
                   , ("west from clearing 2", "clearing 2")
                   , ("down from canyon view", "canyon view")
                   , ("west from canyon view", "canyon view")
                   , ("northwest from canyon view", "canyon view")
                   , ("up from rocky ledge", "rocky ledge")
                   , ("down from rocky ledge", "rocky ledge")
                   , ("up from canyon bottom", "canyon bottom")
                   , ("north from canyon bottom", "canyon bottom")
                   , ("southwest from rainbow", "end of rainbow")
                   , ("north from forest 3", "forest 3")
                   , ("west from forest 3", "forest 3")
                   , ("northwest from forest 3", "forest 3")
                   , ("up from kitchen", "kitchen")
                   , ("east from kitchen", "kitchen")
                   , ("west from kitchen", "kitchen")
                   , ("brownSack", "kitchen")
                   , ("bottle", "kitchen")
                   , ("down from attic", "attic dark")
                   , ("east from living room", "living room")
                   , ("trophyCase", "living room")
                   , ("brassLamp", "living room")
                   , ("sword", "living room")
                   , ("rug", "living room")
                   , ("up from kitchen", "hidden")
                   , ("west from behind house", "hidden")
                   , ("leaflet", "hidden")
                   , ("lunch", "hidden")
                   , ("garlic", "hidden")
                   , ("quantityOfWater", "hidden")
                   , ("rope", "hidden")
                   , ("nastyKnife", "hidden")] Map.empty)


-- | Special Rooms - for hidden objects, and inventory objects.
-- 
inventory = location "inventory" ("You are carrying:" ++ "\n")
hidden    = location "" ""

-- | Rooms - starting and outside of house.
-- 
westOfHouse  = location "west of house" "You are standing in an open field west of a white house, with a boarded front door."
northOfHouse = location "north of house" "You are facing the north side of a white house. There is no door here, and all the windows are boarded up. To the north a narrow path winds through the trees."
behindHouse  = location "behind house" "You are behind the white house."
southOfHouse = location "south of house" "You are facing the south side of a white house. There is no door here, and all the windows are boarded up."
forest1      = location "forest 1" "This is a forest, with trees in all directions. To the east, there appears to be sunlight."
clearing1    = location "clearing 1" "You are in a clearing, with a forest surrounding you on all sides. A path leads south."
forest3      = location "forest 3" "This is a dimly lit forest, with large trees all around."
forestPath   = location "forest path" "This is a path winding through a dimly lit forest. The path heads north-south here."
forest2      = location "forest 2" "This is a dimly lit forest with large trees all atound."
upTree       = location "up a tree" ("You are about 10 feet above the ground nested amont some large branches. The nearest branch above you is above your reach." ++ "\n" ++
                    " Beside you on the branch is a small bird's nest." ++ "\n" ++
                    " In the bird's nest is a large egg encrusted with precious jewels, apparently scavenged by a childless songbord. The egg is covered with fine gold inlay, and ornamented in lapis lazuli and mother-of-pearl. Unlike most eggs, this one is hinged and closed with a delicate looking clasp. The egg appears extremely fragile.")
clearing2    = location "clearing 2" "You are in a small clearing in a well marked forest path that extends to the east and west."
forest4      = location "forest 4" "The forest thins out, revealing impassable mountains."
canyonView   = location "canyon view" "You are at the top of the Great Canyon on its west wall. From here there is a marvelous view of the canyon and parts of the Frigid River upstream. Across the canyon, the walls of the White Cliffs join the mighty ramparts of the Flathead Mountains to the east. Following the Canyon upstream to the north, Aragain Falls may be seen, complete with rainbow. The mighty Frigid River flows out from a great dark cavern. To the west and south can be seen an immense forest, stretching for miles around."
rockyLedge   = location "rocky ledge" "You are on a ledge about halfway up the wall of the river canyon. You can see from here that the main flow from Aragain Falls twists along a passage which it is impossible for you to enter."
canyonBottom = location "canyon bottom" "The lesser part of the runoff of Aragain Falls flows by below."
endOfRainbow = location "end of rainbow" "You are on a small, rocky beach on the contunuation of the Frigid River past the Falls. The beach is narrow due to the presence of the White Cliffs. The river canyon opens here and sunlight shines in from above."


-- | Rooms - inside of house.
-- 
kitchen    = location "kitchen" "You are in the kitchen of the white house. A table seems to have been used recently for the preparation of food."
atticDark  = location "dark attic" ("You have moved into a dark place." ++ "\n" ++ "It is pitch black. You are likely to be eaten by a grue.")
attic      = location "attic" "This is the attic."
livingRoom = location "living room" "You are in the living room."


-- | Exits by Room - starting, and outside of house. Constructed as objects, and placed as a list of strings in each room, since these do not change.
-- 
fromWestOfHouseToNorthOfHouse = exit "north from west of house" ["n"] "A path leads around the house to the north." (basicMove northOfHouse)
fromWestOfHouseToSouthOfHouse = exit "south from west of house" ["s"] "A path leads around the house to the south." (basicMove southOfHouse)
fromWestOfHouseToForest1      = exit "west from west of house" ["w"] "A path leads away from the house to the east into a dense forest." (basicMove forest1)
fromNorthOfHouseToForestPath  = exit "north from north of house" ["n"] "A path leads away from the house to the north toward an open forest path." (basicMove forestPath)
fromNorthOfHouseToBehindHouse = exit "east from north of house" ["e"] "A path leads around the house to the east." (basicMove behindHouse)
fromNorthOfHouseToWestOfHouse = exit "west from north of house" ["w"] "A path leads around the house to the west." (basicMove westOfHouse)
fromBehindHouseToNorthOfHouse = exit "north from behind house" ["n"] "A path leads around the house to the north." (basicMove northOfHouse)
fromBehindHouseToClearing2    = exit "east from behind house" ["e"] "A path leads away from the house to the east into a clearing." (basicMove clearing2)
fromBehindHouseToSouthOfHouse = exit "south from behind house" ["s"] "A path leads around the house to the south." (basicMove southOfHouse)
fromBehindHouseToKitchen      = exit "west from behind house" ["w"] "In one corner of the house there is a small window which is slightly ajar." (basicMove kitchen)
fromSouthOfHouseToBehindHouse = exit "east from south of house" ["e"] "A path leads around the house to the east." (basicMove behindHouse)
fromSouthOfHouseToForest3     = exit "south from south of house" ["s"] "A path leads away from the house to the south into a dense forest." (basicMove forest3)
fromSouthOfHouseToWestOfHouse = exit "west from south of house" ["w"] "A path leads around the house to the west." (basicMove westOfHouse)
fromForest1ToClearing1        = exit "north from forest 1" ["n"] "You hear in the distance the chirping of a song bird." (basicMove clearing1)
fromForest1ToForestPath       = exit "east from forest 1" ["e"] "A path leads to the east toward a forest path." (basicMove forestPath)
fromForest1ToForest3          = exit "south from forest 1" ["s"] "A path leads to the south into more forest." (basicMove forest3)
fromClearing1ToForest2        = exit "east from clearing 1" ["e"] "A path leads to the east into a dense forest." (basicMove forest2)
fromClearing1ToForestPath     = exit "south from clearing 1" ["s"] "A path leads to the south onto a forest path." (basicMove forestPath)
fromClearing1ToForest1        = exit "west from clearing 1" ["w"] "A path leads to the west into a dense forest." (basicMove forest1)
fromForestPathToClearing1     = exit "north from forest path" ["n"] "A path leads to the north into a clearing." (basicMove clearing1)
fromForestPathUpTree          = exit "up from forest path" ["u"] "One particularly large tree with some low branches stands at the edge of the path." (basicMove upTree)
fromForestPathToForest2       = exit "east from forest path" ["e"] "A path leads to the east into a dense forest." (basicMove forest2)
fromForestPathToNorthOfHouse  = exit "south from forest path" ["s"] "A path leads to the south toward a white house." (basicMove northOfHouse)
fromForestPathToForest1       = exit "west from forest path" ["w"] "A path leads to the west into a dense forest." (basicMove forest1)
fromUpTreeToForestPath        = exit "down from up tree" ["d"] "You hear in the distance the chirping of a song bird." (basicMove forestPath)
fromForest2ToForest4          = exit "east from forest 2" ["e"] "A path leads to the east into more forest." (basicMove forest4)
fromForest2ToClearing2        = exit "south from forest 2" ["s"] "A path leads to the south into a clearing." (basicMove clearing2)
fromForest2ToForestPath       = exit "west from forest 2" ["w"] "A path leads to the west onto a forest path." (basicMove forestPath)
fromForest4ToForest2N         = exit "north from forest 4" ["n"] "A path leads back into the forest." (basicMove forest2)
fromForest4ToForest2E         = exit "east from forest 4" ["e"] "A path leads back into the forest." (basicMove forest2)
fromForest4ToForest2S         = exit "south from forest 4" ["s"] "A path leads back into the forest." (basicMove forest2)
fromClearing2ToForest2        = exit "north from clearing 2" ["n"] "A path leads to the north into a dense forest." (basicMove forest2)
fromClearing2ToCanyonView     = exit "east from clearing 2" ["e"] "A path leads to the east toward a large canyon." (basicMove canyonView)
fromClearing2ToForest3        = exit "south from clearing 2" ["s"] "A path leads to the south into a dense forest." (basicMove forest3)
fromClearing2ToBehindHouse    = exit "west from clearing 2" ["w"] "A path leads to the west toward a white house." (basicMove behindHouse)
fromCanyonViewToRockyLedge    = exit "down from canyon view" ["d", "east", "e"] "It is possible to climb down into the canyon from here." (basicMove rockyLedge)
fromCanyonViewToForest3       = exit "west from canyon view" ["w"] "A dense forest lies to the west." (basicMove forest3)
fromCanyonViewToClearing2     = exit "northwest from canyon view" ["nw"] "A forest clearing lies to the northwest." (basicMove clearing2)
fromRockyLedgeToCanyonView    = exit "up from rocky ledge" ["u"] "Above you is more cliff, which appears climbable." (basicMove canyonView)
fromRockyLedgeToCanyonBottom  = exit "down from rocky ledge" ["d"] "Below you is the canyon bottom." (basicMove canyonBottom)
fromCanyonBottomToRockyLedge  = exit "up from canyon bottom" ["u"] "You are beneath the walls of the river canyon which may be climbable here." (basicMove rockyLedge)
fromCanyonBottomToRainbow     = exit "north from canyon bottom" ["n"] "To the north is a narrow path." (basicMove endOfRainbow)
fromRainbowToCanyonBottom     = exit "southwest from rainbow" ["sw"] "A rainbow crosses over the falls to the east and a narrow path continues to the southwest." (basicMove canyonBottom)
fromForest3ToClearing2        = exit "north from forest 3" ["n"] "A path leads to the north into a clearing" (basicMove clearing2)
fromForest3ToForest1          = exit "west from forest 3" ["w"] "A path leads to the west into more forest." (basicMove forest1)
fromForest3ToSouthOfHouse     = exit "northwest from forest 3" ["nw"] "A path leads to the northwest toward a white house." (basicMove southOfHouse)

-- | Exits by Location - inside house. Constructed as objects, and placed as a list of strings in each room, since these do not change.
-- 
fromKitchenToAtticDark   = exit "up from kitchen" ["u"] "A dark staircase can be seen leading upward." (basicMove atticDark)
fromKitchenToAttic       = exit "up from kitchen" ["u"] "A dark staircase can be seen leading upward." (basicMove attic)
fromKitchenToBehindHouse = exit "east from kitchen" ["e"] "To the east is a small window which is open." (basicMove behindHouse)
fromKitchenToLivingRoom  = exit "west from kitchen" ["w"] "A passage leads to the west." (basicMove livingRoom)
fromAtticToKitchen       = exit "down from attic" ["d"] "The only object is the stairway leading down." (basicMove kitchen)
fromLivingRoomToKitchen  = exit "east from living room" ["e"] "There is a doorway to the east." (basicMove kitchen)


-- | Objects some can have actions completed using/on them, and some are worth points, and some have "useful" lives (i.e. can be used for a certain number of turns).
-- 
mailbox          = object "mailbox" ["small mailbox"] "A small mailbox."  useMailbox
leaflet          = object "leaflet" [] "an ordinary useless leaflet" useLeaflet
jeweledEgg       = object "jewel-encrusted egg" ["egg"] "The jewel-encrusted egg is closed." useEgg
leaves           = object "pile of leaves" ["leaves"] "There's nothing special about the pile of leaves." useLeaves
brownSack        = object "elongated brown sack, smelling of hot peppers" ["sack", "brown sack"] "" useBrownSack
lunch            = object "lunch" [] "" useLunch
garlic           = object "clove of garlic" ["garlic"] "" useGarlic
bottle           = object "glass bottle" ["bottle"] "The glass bottle contains: A quantity of water" useBottle
rope             = object "large coil of rope" ["rope"] "" useRope
nastyKnife       = object "nasty-looking knife" ["nasty knife", "knife"] "" useKnife
trophyCase       = object "trophy case" ["case"] "" useTrophyCase
brassLamp        = object "brass lamp" ["lamp"] "" useBrassLamp
sword            = object "an elvish sword of great antiquity" ["sword", "ancient sword", "sword of antiquity"] "There's nothing special about the sword." useSword
rug              = object "large oriental rug" ["rug"] "" useRug
window           = object "a small window which is slightly ajar" ["window"] "" useWindow
quantityOfWater  = object "a quantity of water" ["water"] "" useQuantityOfWater


-- | Use Objects functions.
-- 
useMailbox :: Action -> State World [String]
useMailbox Close  = singleAnswer "Closed."
useMailbox Kill   = singleAnswer "I've known some strange people, but fighting a small mailbox?"
useMailbox Open   = do get >>= \wrld -> put wrld{ objectLocations = Map.insert "leaflet" "west of house" (objectLocations wrld) } >> singleAnswer "Opening the small mailbox reveals a leaflet."
useMailbox Search = singleAnswer "It's right here."
useMailbox Take   = singleAnswer "It is securely anchored."
useMailbox Talk   = singleAnswer "You can't talk to the small mailbox!"
useMailbox _      = singleAnswer "You can't do that with the small mailbox!"

useLeaflet :: Action -> State World [String]
useLeaflet Consume = singleAnswer "I don't think that the leaflet would agree with you."
useLeaflet Drop    = singleAnswer "Dropped."
useLeaflet Examine = singleAnswer ("WELCOME TO ZORKELL!" ++ "\n\n" ++ "ZORKELL is a game (based on Zork and written in Haskell) of adventure, danger, and low cunning. In it you will explore some of the most amazing territory ever seen by mortals. No computer shoud be without one!")
useLeaflet Kill    = singleAnswer "I've known some strange people, but fighting a leaflet?"
useLeaflet Take    = do get >>= \wrld -> put wrld{ objectLocations = Map.insert "leaflet" "inventory" (objectLocations wrld) } >> singleAnswer "Taken."
useLeaflet _       = singleAnswer "You can't do that with a leaflet!"

useEgg :: Action -> State World [String]
useEgg Consume = singleAnswer "I don't think that the jewel-encrusted egg would agree with you."
useEgg Drop    = singleAnswer "The egg falls to the ground and springs open, seriously damaged. There is a golden clockwork canary nestled in the egg. It seems to have recently had a bad experience. The mountings for its jewel-like eyes are empty, and its silver beak is crumpled. Through a cracked crystal window below its left wing you can see the remains of intricate machinery. It is not clear what result winding it would have, as the mainspring seems sprung."
useEgg Kill    = singleAnswer "I've known some strange people, but fighting a jewel-encrusted egg?"
useEgg Open    = singleAnswer "You have neither the tools nore the expertise."
useEgg Take    = singleAnswer "Taken."
useEgg Talk    = singleAnswer "You can't talk to the broken jewel-encrusted egg!"
useEgg _       = singleAnswer "You can't do that with the jewel-encrusted egg!"

useLeaves :: Action -> State World [String]
useLeaves Consume = singleAnswer "I don't think the pile of leaves would agree with you."
-- useLeaves Move    = singleAnswer "Done." ++ "\n" ++ "In disturbing the pile of leaves, a grating is revealed."
useLeaves _       = singleAnswer "You must tell me how to do that to a pile of leaves."

useBrownSack :: Action -> State World [String]
useBrownSack Close   = singleAnswer "Closed."
useBrownSack Consume = singleAnswer "I don't think the brown sack would agree with you."
useBrownSack Drop    = singleAnswer "Dropped."
useBrownSack Open    = singleAnswer "Opening the brown sack reveals a lunch, and a clove of garlic."
useBrownSack Take    = singleAnswer "Taken."
useBrownSack _       = singleAnswer "You can't do that with the elongated brown sack."

useGarlic :: Action -> State World [String]
useGarlic Consume = singleAnswer "Spicy."
useGarlic Drop    = singleAnswer "Dropped."
useGarlic Take    = singleAnswer "Taken."
useGarlic _       = singleAnswer "You can't do that with a clove of garlic."

useLunch :: Action -> State World [String]
useLunch Consume = singleAnswer "Thank you, that hit the spot."
useLunch Drop    = singleAnswer "Dropped."
useLunch Take    = singleAnswer "Taken."
useLunch _       = singleAnswer "You'll have to tell me how to do that with the lunch."

useBottle :: Action -> State World [String]
useBottle Close   = singleAnswer "Closed."
useBottle Consume = singleAnswer "I don't think the glass bottle would agree with you."
useBottle Open    = singleAnswer "Opened."
useBottle _       = singleAnswer "You can't do that with the glass bottle."

useRope :: Action -> State World [String]
useRope Take = singleAnswer "Taken."
useRope _    = singleAnswer "You can't do that with the rope."

useKnife :: Action -> State World [String]
useKnife Take = singleAnswer "Taken."
useKnife _    = singleAnswer "You can't do that with the knife."

useTrophyCase :: Action -> State World [String]
useTrophyCase Close   = singleAnswer "Closed."
useTrophyCase Consume = singleAnswer "I don't think that the trophy case would agree with you."
useTrophyCase Open    = singleAnswer "Opened."
useTrophyCase _       = singleAnswer "You can't do that with the trophy case."

useBrassLamp :: Action -> State World [String]
useBrassLamp Consume = singleAnswer "I don't think the brass lantern would agree with you."
useBrassLamp Drop    = singleAnswer "Dropped."
useBrassLamp Take    = singleAnswer "Taken."
useBrassLamp TurnOff = singleAnswer "The brass lantern is now off."
useBrassLamp TurnOn  = singleAnswer "The brass lantern is now on."
useBrassLamp _       = singleAnswer "You can't do that with the brass lantern."

useSword :: Action -> State World [String]
useSword Drop = singleAnswer "Dropped."
useSword Put  = singleAnswer "Done."
useSword Take = singleAnswer "Taken."
useSword _    = singleAnswer "You can't do that with the sword."

useRug :: Action -> State World [String]
useRug Consume = singleAnswer "I don't think that the oriental rug would agree with you."
useRug Take    = singleAnswer "The rug is extremely heavy and can't be carried."
-- useRug Move = singleAnswer "With great effort the rug is moved to one side of the room, revealing the dusty cover of a closed trap door."
useRug _       = singleAnswer "You can't do that with the rug."

useWindow :: Action -> State World [String]
useWindow Close = singleAnswer "The window closes (more easily than it opened)."
useWindow Open  = singleAnswer "With great effort, you open the window far enough to allow entry."
useWindow _     = singleAnswer "You can't do that with the window."

useQuantityOfWater :: Action -> State World [String]
useQuantityOfWater Consume = singleAnswer "Ah, thank you, I was thirsty. Probably from all this talking."
useQuantityOfWater _       = singleAnswer "You'll have to tell me how to do that with the water."