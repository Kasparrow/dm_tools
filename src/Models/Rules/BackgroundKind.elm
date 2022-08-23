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
    | NoBackground


type alias BackgroundKinds =
    List BackgroundKind


all : BackgroundKinds
all =
    [ NoBackground
    , Acolyte
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


fromString : String -> BackgroundKind
fromString string =
    case string of
        "Acolyte" ->
            Acolyte

        "Charlatan" ->
            Charlatan

        "Criminal" ->
            Criminal

        "Entertainer" ->
            Entertainer

        "FolkHero" ->
            FolkHero

        "GuildArtisan" ->
            GuildArtisan

        "Hermit" ->
            Hermit

        "Noble" ->
            Noble

        "Outlander" ->
            Outlander

        "Sage" ->
            Sage

        "Sailor" ->
            Sailor

        "Soldier" ->
            Soldier

        "Urchin" ->
            Urchin

        _ ->
            NoBackground
