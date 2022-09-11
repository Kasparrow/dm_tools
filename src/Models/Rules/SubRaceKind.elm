module Models.Rules.SubRaceKind exposing (SubRaceKind(..), SubRaceKinds, all, fromString)


type SubRaceKind
    = BlackDragonborn
    | BlueDragonborn
    | BrassDragonborn
    | BronzeDragonborn
    | CopperDragonborn
    | GoldDragonborn
    | GreenDragonborn
    | RedDragonborn
    | SilverDragonborn
    | WhiteDragonborn
    | HillsDwarf
    | MountainsDwarf
    | HighElf
    | WoodElf
    | Drow
    | DeepGnome
    | RockGnome
    | LightfootHalfling
    | StoutHalfling


type alias SubRaceKinds =
    List SubRaceKind


all : SubRaceKinds
all =
    [ BlackDragonborn
    , BlueDragonborn
    , BrassDragonborn
    , BronzeDragonborn
    , CopperDragonborn
    , GoldDragonborn
    , GreenDragonborn
    , RedDragonborn
    , SilverDragonborn
    , WhiteDragonborn
    , HillsDwarf
    , MountainsDwarf
    , HighElf
    , WoodElf
    , Drow
    , DeepGnome
    , RockGnome
    , LightfootHalfling
    , StoutHalfling
    ]


fromString : String -> Maybe SubRaceKind
fromString string =
    case string of
        "BlackDragonborn" ->
            Just BlackDragonborn

        "BlueDragonborn" ->
            Just BlueDragonborn

        "BrassDragonborn" ->
            Just BrassDragonborn

        "BronzeDragonborn" ->
            Just BronzeDragonborn

        "CopperDragonborn" ->
            Just CopperDragonborn

        "GoldDragonborn" ->
            Just GoldDragonborn

        "GreenDragonborn" ->
            Just GreenDragonborn

        "RedDragonborn" ->
            Just RedDragonborn

        "SilverDragonborn" ->
            Just SilverDragonborn

        "WhiteDragonborn" ->
            Just WhiteDragonborn

        "HillsDwarf" ->
            Just HillsDwarf

        "MountainsDwarf" ->
            Just MountainsDwarf

        "Drow" ->
            Just Drow

        "HighElf" ->
            Just HighElf

        "WoodElf" ->
            Just WoodElf

        "DeepGnome" ->
            Just DeepGnome

        "RockGnome" ->
            Just RockGnome

        "LightfootHalfling" ->
            Just LightfootHalfling

        "StoutHalfling" ->
            Just StoutHalfling

        _ ->
            Nothing
