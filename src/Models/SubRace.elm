module Models.SubRace exposing (SubRace, SubRaces, get)

import Models.StatKind exposing(StatKind(..))
import Models.SubRaceKind exposing(SubRaceKind(..))
import Models.Stat exposing (Stats)
import Models.RuleSetKind exposing (RuleSetKind(..), RuleSetKinds)

type alias SubRace =
    { subRaceKind: SubRaceKind
    , statBonus: Stats
    , ruleSetKinds: RuleSetKinds
    , asString: String
    }

type alias SubRaces = List SubRace

get: SubRaceKind -> SubRace
get subRaceKind =
    case subRaceKind of
        BlackDragonborn ->
            { subRaceKind = BlackDragonborn
            , statBonus = []
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Black Dragonborn"
            }
        BlueDragonborn ->
            { subRaceKind = BlueDragonborn
            , statBonus = []
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Blue Dragonborn"
            }
        BrassDragonborn ->
            { subRaceKind = BrassDragonborn
            , statBonus = []
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Brass Dragonborn"
            }
        BronzeDragonborn ->
            { subRaceKind = BronzeDragonborn 
            , statBonus = []
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Bronze Dragonborn"
            }
        CopperDragonborn ->
            { subRaceKind = CopperDragonborn
            , statBonus = []
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Copper Dragonborn"
            }
        GoldDragonborn ->
            { subRaceKind = GoldDragonborn 
            , statBonus = []
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Gold Dragonborn"
            }
        GreenDragonborn ->
            { subRaceKind = GreenDragonborn
            , statBonus = []
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Green Dragonborn"
            }
        RedDragonborn ->
            { subRaceKind = RedDragonborn
            , statBonus = []
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Red Dragonborn"
            }
        SilverDragonborn ->
            { subRaceKind = SilverDragonborn
            , statBonus = []
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Silver Dragonborn"
            }
        WhiteDragonborn ->
            { subRaceKind = WhiteDragonborn
            , statBonus = []
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "White Dragonborn"
            }
        HillsDwarf ->
            { subRaceKind = HillsDwarf
            , statBonus = [ (Wisdom, 1) ]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Hills Dwarf"
            }
        MountainsDwarf ->
            { subRaceKind = MountainsDwarf
            , statBonus = [ (Strength, 1) ]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Mountains Dwarf"
            }
        Drow ->
            { subRaceKind = Drow
            , statBonus = [ (Charisma, 1) ]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Drow"
            }
        WoodElf ->
            { subRaceKind = WoodElf
            , statBonus = [ (Wisdom, 1) ]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Wood Elf"
            }
        HighElf ->
            { subRaceKind = HighElf
            , statBonus = [ (Intelligence, 1) ]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "High Elf"
            }
        DeepGnome ->
            { subRaceKind = DeepGnome
            , statBonus = [ (Dexterity, 1) ]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Deep Gnome"
            }
        RockGnome ->
            { subRaceKind = RockGnome
            , statBonus = [ (Constitution, 1) ]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Rock Gnome"
            }
        LightfootHalfling ->
            { subRaceKind = LightfootHalfling
            , statBonus = [ (Charisma, 1) ]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Lightfoot Halfling"
            }
        StoutHalfling ->
            { subRaceKind = StoutHalfling
            , statBonus = [ (Constitution, 1) ]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Stout Halfling"
            }
        NoSubRace ->
            { subRaceKind = NoSubRace
            , statBonus = []
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = ""
            }
