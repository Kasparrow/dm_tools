module Models.Stat exposing (Stat, Stats)

import Models.StatKind exposing (StatKind)

type alias Stat = (StatKind, Int)
type alias Stats = List Stat
