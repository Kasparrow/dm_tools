module Models.Rules.StatKind exposing (StatKind(..), StatKinds, all, toString)

type StatKind
    = Strength
    | Dexterity
    | Constitution
    | Intelligence
    | Wisdom
    | Charisma

type alias StatKinds = List StatKind

all: StatKinds
all = 
    [ Strength, Dexterity, Constitution, Intelligence, Wisdom, Charisma ]

toString: StatKind -> String
toString statKind =
    case statKind of
        Strength -> "STR"
        Dexterity -> "DEX"
        Constitution -> "CON"
        Intelligence -> "INT"
        Wisdom -> "WIS"
        Charisma -> "CHA"
