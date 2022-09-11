module Models.Rules.SubRace exposing (SubRace, SubRaces, get, none)

import Models.Rules.RuleSetKind exposing (RuleSetKind(..), RuleSetKinds)
import Models.Rules.Stat exposing (Stats)
import Models.Rules.StatKind exposing (StatKind(..))
import Models.Rules.SubRaceKind exposing (SubRaceKind(..))


type alias SubRace =
    { statBonus : Stats
    , asString : String
    }


type alias SubRaces =
    List SubRace


get : SubRaceKind -> SubRace
get subRaceKind =
    case subRaceKind of
        BlackDragonborn ->
            { statBonus = []
            , asString = "Black Dragonborn"
            }

        BlueDragonborn ->
            { statBonus = []
            , asString = "Blue Dragonborn"
            }

        BrassDragonborn ->
            { statBonus = []
            , asString = "Brass Dragonborn"
            }

        BronzeDragonborn ->
            { statBonus = []
            , asString = "Bronze Dragonborn"
            }

        CopperDragonborn ->
            { statBonus = []
            , asString = "Copper Dragonborn"
            }

        GoldDragonborn ->
            { statBonus = []
            , asString = "Gold Dragonborn"
            }

        GreenDragonborn ->
            { statBonus = []
            , asString = "Green Dragonborn"
            }

        RedDragonborn ->
            { statBonus = []
            , asString = "Red Dragonborn"
            }

        SilverDragonborn ->
            { statBonus = []
            , asString = "Silver Dragonborn"
            }

        WhiteDragonborn ->
            { statBonus = []
            , asString = "White Dragonborn"
            }

        HillsDwarf ->
            { statBonus = [ ( Wisdom, 1 ) ]
            , asString = "Hills Dwarf"
            }

        MountainsDwarf ->
            { statBonus = [ ( Strength, 1 ) ]
            , asString = "Mountains Dwarf"
            }

        Drow ->
            { statBonus = [ ( Charisma, 1 ) ]
            , asString = "Drow"
            }

        WoodElf ->
            { statBonus = [ ( Wisdom, 1 ) ]
            , asString = "Wood Elf"
            }

        HighElf ->
            { statBonus = [ ( Intelligence, 1 ) ]
            , asString = "High Elf"
            }

        DeepGnome ->
            { statBonus = [ ( Dexterity, 1 ) ]
            , asString = "Deep Gnome"
            }

        RockGnome ->
            { statBonus = [ ( Constitution, 1 ) ]
            , asString = "Rock Gnome"
            }

        LightfootHalfling ->
            { statBonus = [ ( Charisma, 1 ) ]
            , asString = "Lightfoot Halfling"
            }

        StoutHalfling ->
            { statBonus = [ ( Constitution, 1 ) ]
            , asString = "Stout Halfling"
            }


none =
    { statBonus = []
    , asString = ""
    }
