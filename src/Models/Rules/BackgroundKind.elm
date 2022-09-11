module Models.Rules.BackgroundKind exposing (BackgroundKind(..), BackgroundKinds, all, fromString)


type BackgroundKind
    = Acolyte
    | Charlatan
    | Criminal
    | Entertainer
    | FolkHero
    | GuildArtisan
    | Hermit
    | Noble
    | Outlander
    | Sage
    | Sailor
    | Soldier
    | Urchin


type alias BackgroundKinds =
    List BackgroundKind


all : BackgroundKinds
all =
    [ Acolyte
    , Charlatan
    , Criminal
    , Entertainer
    , FolkHero
    , GuildArtisan
    , Hermit
    , Noble
    , Outlander
    , Sage
    , Sailor
    , Soldier
    , Urchin
    ]


fromString : String -> Maybe BackgroundKind
fromString string =
    case string of
        "Acolyte" ->
            Just Acolyte

        "Charlatan" ->
            Just Charlatan

        "Criminal" ->
            Just Criminal

        "Entertainer" ->
            Just Entertainer

        "FolkHero" ->
            Just FolkHero

        "GuildArtisan" ->
            Just GuildArtisan

        "Hermit" ->
            Just Hermit

        "Noble" ->
            Just Noble

        "Outlander" ->
            Just Outlander

        "Sage" ->
            Just Sage

        "Sailor" ->
            Just Sailor

        "Soldier" ->
            Just Soldier

        "Urchin" ->
            Just Urchin

        _ ->
            Nothing
