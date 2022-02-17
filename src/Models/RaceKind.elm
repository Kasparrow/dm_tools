module Models.RaceKind exposing (RaceKind(..), all, fromString)

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
type alias RaceKinds = List RaceKind

