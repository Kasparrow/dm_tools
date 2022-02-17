module Models.SubRaceKind exposing (SubRaceKind(..), SubRaceKinds, all, fromString)

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
    | NoSubRace

type alias SubRaceKinds = List SubRaceKind

all: SubRaceKinds
all =
    [ NoSubRace
    , BlackDragonborn
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

fromString: String -> SubRaceKind
fromString string =
    case string of
        "BlackDragonborn" -> BlackDragonborn
        "BlueDragonborn" -> BlueDragonborn
        "BrassDragonborn" -> BrassDragonborn
        "BronzeDragonborn" -> BronzeDragonborn
        "CopperDragonborn" -> CopperDragonborn
        "GoldDragonborn" -> GoldDragonborn
        "GreenDragonborn" -> GreenDragonborn
        "RedDragonborn" -> RedDragonborn
        "SilverDragonborn" -> SilverDragonborn
        "WhiteDragonborn" -> WhiteDragonborn
        "HillsDwarf" -> HillsDwarf
        "MountainsDwarf" -> MountainsDwarf
        "Drow" -> Drow
        "HighElf" -> HighElf
        "WoodElf" -> WoodElf
        "DeepGnome" -> DeepGnome
        "RockGnome" -> RockGnome
        "LightfootHalfling" -> LightfootHalfling
        "StoutHalfling" -> StoutHalfling
        _ -> NoSubRace
