module Models.StatKind exposing (StatKind(..), StatKinds, all)

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
