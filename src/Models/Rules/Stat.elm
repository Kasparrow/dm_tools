module Models.Rules.Stat exposing (Stat, Stats)

import Models.Rules.StatKind exposing (StatKind)


type alias Stat =
    ( StatKind, Int )


type alias Stats =
    List Stat
