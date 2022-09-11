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


type alias ClassKinds =
    List ClassKind


all : ClassKinds
all =
    [ Barbarian
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


fromString : String -> Maybe ClassKind
fromString string =
    case string of
        "Barbarian" ->
            Just Barbarian

        "Bard" ->
            Just Bard

        "Cleric" ->
            Just Cleric

        "Druid" ->
            Just Druid

        "Fighter" ->
            Just Fighter

        "Monk" ->
            Just Monk

        "Paladin" ->
            Just Paladin

        "Ranger" ->
            Just Ranger

        "Rogue" ->
            Just Rogue

        "Sorcerer" ->
            Just Rogue

        "Warlock" ->
            Just Warlock

        "Wizard" ->
            Just Wizard

        "Scholar" ->
            Just Scholar

        "Slayer" ->
            Just Slayer

        "TreasureHunter" ->
            Just TreasureHunter

        "Wanderer" ->
            Just Wanderer

        "Warden" ->
            Just Warden

        "Warrior" ->
            Just Warrior

        _ ->
            Nothing
