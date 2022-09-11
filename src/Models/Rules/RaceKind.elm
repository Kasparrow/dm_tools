module Models.Rules.RaceKind exposing (RaceKind(..), RaceKinds, all, fromString)


type RaceKind
    = Dragonborn
    | Dwarf
    | Elf
    | Gnome
    | HalfElf
    | Halfling
    | HalfOrc
    | Human
    | Tiefling
    | Barding
    | Beorning
    | Dunedain
    | LonelyMountainDwarf
    | MirkwoodElf
    | ShireHobbit
    | BreeMen
    | LakeMen
    | MinasTirithMen
    | RohanRider
    | WilderlandWoodmen


type alias RaceKinds =
    List RaceKind


all : RaceKinds
all =
    [ Dragonborn
    , Dwarf
    , Elf
    , Gnome
    , HalfElf
    , Halfling
    , HalfOrc
    , Human
    , Tiefling
    , Barding
    , Beorning
    , Dunedain
    , LonelyMountainDwarf
    , MirkwoodElf
    , ShireHobbit
    , BreeMen
    , LakeMen
    , MinasTirithMen
    , RohanRider
    , WilderlandWoodmen
    ]


fromString : String -> Maybe RaceKind
fromString string =
    case string of
        "Dragonborn" ->
            Just Dragonborn

        "Dwarf" ->
            Just Dwarf

        "Elf" ->
            Just Elf

        "Gnome" ->
            Just Gnome

        "HalfElf" ->
            Just HalfElf

        "Halfling" ->
            Just Halfling

        "HalfOrc" ->
            Just HalfOrc

        "Human" ->
            Just Human

        "Tiefling" ->
            Just Tiefling

        "Barding" ->
            Just Barding

        "Beorning" ->
            Just Beorning

        "Dunedain" ->
            Just Dunedain

        "LonelyMountainDwarf" ->
            Just LonelyMountainDwarf

        "MirkwoodElf" ->
            Just MirkwoodElf

        "ShireHobbit" ->
            Just ShireHobbit

        "BreeMen" ->
            Just BreeMen

        "LakeMen" ->
            Just LakeMen

        "MinasTirithMen" ->
            Just MinasTirithMen

        "RohanRider" ->
            Just RohanRider

        "WilderlandWoodmen" ->
            Just WilderlandWoodmen

        _ ->
            Nothing
