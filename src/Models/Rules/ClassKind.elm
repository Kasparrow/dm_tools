module Models.Rules.ClassKind exposing (ClassKind(..), ClassKinds, all, fromString)


type ClassKind
    = Barbarian
    | Bard
    | Cleric
    | Druid
    | Fighter
    | Monk
    | Paladin
    | Ranger
    | Rogue
    | Sorcerer
    | Warlock
    | Wizard
    | Scholar
    | Slayer
    | TreasureHunter
    | Wanderer
    | Warden
    | Warrior
    | NoClass


type alias ClassKinds =
    List ClassKind


all : ClassKinds
all =
    [ NoClass
    , Barbarian
    , Bard
    , Cleric
    , Druid
    , Fighter
    , Monk
    , Paladin
    , Ranger
    , Rogue
    , Sorcerer
    , Warlock
    , Wizard
    , Scholar
    , Slayer
    , TreasureHunter
    , Wanderer
    , Warden
    , Warrior
    ]


fromString : String -> ClassKind
fromString string =
    case string of
        "Barbarian" ->
            Barbarian

        "Bard" ->
            Bard

        "Cleric" ->
            Cleric

        "Druid" ->
            Druid

        "Fighter" ->
            Fighter

        "Monk" ->
            Monk

        "Paladin" ->
            Paladin

        "Ranger" ->
            Ranger

        "Rogue" ->
            Rogue

        "Sorcerer" ->
            Rogue

        "Warlock" ->
            Warlock

        "Wizard" ->
            Wizard

        "Scholar" ->
            Scholar

        "Slayer" ->
            Slayer

        "TreasureHunter" ->
            TreasureHunter

        "Wanderer" ->
            Wanderer

        "Warden" ->
            Warden

        "Warrior" ->
            Warrior

        _ ->
            NoClass
