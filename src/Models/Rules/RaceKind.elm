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
    | NoRace


type alias RaceKinds =
    List RaceKind


all : RaceKinds
all =
    [ NoRace
    , Dragonborn
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


fromString : String -> RaceKind
fromString string =
    case string of
        "Dragonborn" ->
            Dragonborn

        "Dwarf" ->
            Dwarf

        "Elf" ->
            Elf

        "Gnome" ->
            Gnome

        "HalfElf" ->
            HalfElf

        "Halfling" ->
            Halfling

        "HalfOrc" ->
            HalfOrc

        "Human" ->
            Human

        "Tiefling" ->
            Tiefling

        "Barding" ->
            Barding

        "Beorning" ->
            Beorning

        "Dunedain" ->
            Dunedain

        "LonelyMountainDwarf" ->
            LonelyMountainDwarf

        "MirkwoodElf" ->
            MirkwoodElf

        "ShireHobbit" ->
            ShireHobbit

        "BreeMen" ->
            BreeMen

        "LakeMen" ->
            LakeMen

        "MinasTirithMen" ->
            MinasTirithMen

        "RohanRider" ->
            RohanRider

        "WilderlandWoodmen" ->
            WilderlandWoodmen

        _ ->
            NoRace
