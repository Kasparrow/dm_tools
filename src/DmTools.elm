module DmTools exposing (main)

import Html exposing (Html, div, h1, img, span, text, label, input, select, option, br, a, nav, p, footer, h1, h2, h3, h4, h5, h6)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Array
import Browser

type Msg
    = UpdateRemainingPoints
    | UpdateRace String
    | UpdateSubRace String
    | IncrementStrength
    | DecrementStrength
    | IncrementDexterity
    | DecrementDexterity
    | IncrementConstitution
    | DecrementConstitution
    | IncrementIntelligence
    | DecrementIntelligence
    | IncrementWisdom
    | DecrementWisdom
    | IncrementCharisma
    | DecrementCharisma

init: Model
init = 
    { rolledStats = 
        { strength = 8
        , dexterity = 8
        , constitution = 8
        , intelligence = 8
        , wisdom = 8
        , charisma = 8
        }
    , race = NoRace
    , subRace = NoSubRace
    , remainingPoints = 27
    }

-- MODEL

type alias Stats =
    { strength: Int
    , dexterity: Int
    , constitution: Int
    , intelligence: Int
    , wisdom: Int
    , charisma: Int
    }

type Race 
    = Elf
    | Human
    | Dwarf
    | NoRace

type SubRace
    = HillsDwarf
    | MountainsDwarf
    | HighElf
    | WoodElf
    | Drow
    | NoSubRace

enumRace = 
    [ NoRace
    , Human
    , Dwarf
    , Elf
    ]

getSubRaces: Race -> List SubRace
getSubRaces race =
    case race of
        Elf -> [ NoSubRace, Drow, WoodElf, HighElf ]
        Dwarf -> [ NoSubRace, HillsDwarf, MountainsDwarf ]
        Human -> [ NoSubRace ]
        NoRace -> [ NoSubRace ]

type alias Model = 
    { rolledStats: Stats
    , race: Race
    , subRace: SubRace
    , remainingPoints: Int
    }

-- VIEW

view: Model -> Html Msg
view model =
    let
        raceBonus = (getRaceBonus model.race)
        subRaceBonus = (getSubRaceBonus model.subRace)
    in
    div [ class "main-container" ]
        [ nav []
              [ a [ href "#" ]
                  [ span [ class "navbar-item" ] [ text "Dm Tools" ] ]
              ]
        , div [ class "content" ]
              [ h3 [] [ text "Race" ]
              , viewRaceInput 
              , viewSubRaceInput model.race
              , h3 [] [ text "Rolled stats" ]
              , viewRemainingPoints model
              , div [ class "flex-row" ]
                    [ viewStatInput "STR" model.rolledStats.strength IncrementStrength DecrementStrength
                    , viewStatInput "DEX" model.rolledStats.dexterity IncrementDexterity DecrementDexterity
                    , viewStatInput "CON" model.rolledStats.constitution IncrementConstitution DecrementConstitution
                    , viewStatInput "INT" model.rolledStats.intelligence IncrementIntelligence DecrementIntelligence
                    , viewStatInput "WIS" model.rolledStats.wisdom IncrementWisdom DecrementWisdom
                    , viewStatInput "CHA" model.rolledStats.charisma IncrementCharisma DecrementCharisma
                    ]
              , br [] []
              , h3 [] [ text "Computed stats" ]
              , div [ class "flex-row" ]
                    [ viewStatReader "STR" model.rolledStats.strength raceBonus.strength subRaceBonus.strength
                    , viewStatReader "DEX" model.rolledStats.dexterity raceBonus.dexterity subRaceBonus.dexterity
                    , viewStatReader "CON" model.rolledStats.constitution raceBonus.constitution subRaceBonus.constitution
                    , viewStatReader "INT" model.rolledStats.intelligence raceBonus.intelligence subRaceBonus.intelligence
                    , viewStatReader "WIS" model.rolledStats.wisdom raceBonus.wisdom subRaceBonus.wisdom
                    , viewStatReader "CHA" model.rolledStats.charisma raceBonus.charisma subRaceBonus.charisma
                    ]
--              , viewFinalStat "Strength" model.rolledStats.strength raceBonus.strength subRaceBonus.strength
--              , viewFinalStat "Dexerity" model.rolledStats.dexterity raceBonus.dexterity subRaceBonus.dexterity
--              , viewFinalStat "Constitution" model.rolledStats.constitution raceBonus.constitution subRaceBonus.constitution
--              , viewFinalStat "Intelligence" model.rolledStats.intelligence raceBonus.intelligence subRaceBonus.intelligence
--              , viewFinalStat "Wisdom" model.rolledStats.wisdom raceBonus.wisdom subRaceBonus.wisdom
--              , viewFinalStat "Charisma" model.rolledStats.charisma raceBonus.charisma subRaceBonus.charisma
              ]
        , footer []
                 [ p [] [ text "/[A-Z]IKWAN/" ] ]
        ]

viewRaceInput =
    select [onInput UpdateRace] (List.map viewRaceOption enumRace)

viewRaceOption race =
    case race of
        Elf -> option [ value "Elf" ] [ text "Elf" ]
        Dwarf -> option [ value "Dwarf" ] [ text "Dwarf" ]
        Human -> option [ value "Human" ] [ text "Human" ]
        NoRace -> option [ value "" ] [ text "Select a race" ]

viewSubRaceInput: Race -> Html Msg
viewSubRaceInput currentRace =
    select [onInput UpdateSubRace] (List.map viewSubRaceOption (getSubRaces currentRace))

viewSubRaceOption: SubRace -> Html Msg
viewSubRaceOption subRace =
    case subRace of
        HillsDwarf -> option [ value "HillsDwarf" ] [ text "Hills Dwarf" ]
        MountainsDwarf -> option [ value "MountainsDwarf" ] [ text "Mountains Dwarf" ]
        HighElf -> option [ value "HighElf" ] [ text "High Elf" ]
        WoodElf -> option [ value "WoodElf" ] [ text "Wood Elf" ]
        Drow -> option [ value "Drow" ] [ text "Drow" ]
        NoSubRace -> option [ value "" ] [ text "Select a subrace" ]

viewCharacteristicInput: String -> Int -> (String -> msg) -> Html msg
viewCharacteristicInput name val toMsg =
    div [] 
        [ label [] [ text name ]
        , input [ type_ "number"
                , Html.Attributes.min "8"
                , Html.Attributes.max "15"
                , placeholder name
                , value (String.fromInt val) 
                , onInput toMsg
                ] []
        ]

viewStatInput: String -> Int -> Msg -> Msg -> Html Msg
viewStatInput statName value incrementMsg decrementMsg =
    div [ class "stat-input" ]
        [ span [ class "stat-input-title" ] [ text statName ]
        , div [ class "stat-input-body" ]
              [ span [ class "stat-input-value" ] [ text (String.fromInt value) ]
              , div [ class "stat-input-controls" ]
                    [ span [ onClick incrementMsg ] [ text "+" ]
                    , span [ onClick decrementMsg ] [ text "-" ]
                    ]
              ]
        ]

viewStatReader: String -> Int -> Int -> Int -> Html Msg
viewStatReader statName rolledValue raceBonusValue subRaceBonusValue =
    let
        totalStatValue = rolledValue + raceBonusValue + subRaceBonusValue
    in
    div [ class "stat-reader" ]
        [ span [ class "stat-reader-title" ] [ text statName ]
        , div [ class "stat-reader-body" ]
              [ span [ class "stat-reader-value" ] [ text (String.fromInt totalStatValue) ]
              , div [ class "stat-reader-bonus" ]
                    [ span [] [ text (printWithSign (computeModifier totalStatValue)) ]
                    , span [] []
                    ]
              ]
        ]

viewFinalStat: String -> Int -> Int -> Int ->  Html msg
viewFinalStat name rolledValue raceBonus subRaceBonus =
    let 
        totalStatValue = rolledValue + raceBonus + subRaceBonus
    in
    div [] [ text (name ++ " " ++ (String.fromInt totalStatValue) ++ " (" ++ (printWithSign (computeModifier totalStatValue) ++ ")")) ]

viewRemainingPoints: Model -> Html Msg
viewRemainingPoints model =
    span [] [ text ( "Remaining points : " ++ (String.fromInt model.remainingPoints) )]


-- UPDATE

update: Msg -> Model -> Model
update msg ({rolledStats} as model) =
        case msg of
            IncrementStrength -> 
                update UpdateRemainingPoints { model | rolledStats = { rolledStats | strength = (model.rolledStats.strength + 1) } }
            DecrementStrength -> 
                update UpdateRemainingPoints { model | rolledStats = { rolledStats | strength = (model.rolledStats.strength - 1) } }
            IncrementDexterity -> 
                update UpdateRemainingPoints { model | rolledStats = { rolledStats | dexterity = (model.rolledStats.dexterity + 1) } }
            DecrementDexterity -> 
                update UpdateRemainingPoints { model | rolledStats = { rolledStats | dexterity = (model.rolledStats.dexterity - 1) } }
            IncrementConstitution -> 
                update UpdateRemainingPoints { model | rolledStats = { rolledStats | constitution = (model.rolledStats.constitution + 1) } }
            DecrementConstitution ->
               update UpdateRemainingPoints { model | rolledStats = { rolledStats | constitution = (model.rolledStats.constitution - 1) } }
            IncrementIntelligence -> 
                update UpdateRemainingPoints { model | rolledStats = { rolledStats | intelligence = (model.rolledStats.intelligence + 1) } }
            DecrementIntelligence -> 
                update UpdateRemainingPoints { model | rolledStats = { rolledStats | intelligence = (model.rolledStats.intelligence - 1) } }
            IncrementWisdom -> 
                update UpdateRemainingPoints { model | rolledStats = { rolledStats | wisdom = (model.rolledStats.wisdom + 1) } }
            DecrementWisdom -> 
                update UpdateRemainingPoints { model | rolledStats = { rolledStats | wisdom = (model.rolledStats.wisdom - 1) } }
            IncrementCharisma -> 
                update UpdateRemainingPoints { model | rolledStats = { rolledStats | charisma = (model.rolledStats.charisma + 1) } }
            DecrementCharisma -> 
                update UpdateRemainingPoints { model | rolledStats = { rolledStats | charisma = (model.rolledStats.charisma - 1) } }

            UpdateRemainingPoints ->
                { model | remainingPoints = (27 - (computeRemainingPoints model.rolledStats)) }
            UpdateRace value ->
                { model | race = (stringToRace value), subRace = NoSubRace }
            UpdateSubRace value ->
                { model | subRace = (stringToSubRace value ) }

-- HELPERS

printWithSign: Maybe Int -> String
printWithSign value =
    case value of
        Just int ->
            if int >= 0 then
                "+" ++ (String.fromInt int)
            else
                String.fromInt int
        Nothing ->
            "Stat value cannot be parsed"


computeRemainingPoints: Stats -> Int
computeRemainingPoints stats =
    (computeStatCost stats.strength + computeStatCost stats.dexterity + computeStatCost stats.constitution +
    computeStatCost stats.intelligence + computeStatCost stats.wisdom + computeStatCost stats.charisma)

computeModifier: Int -> Maybe Int
computeModifier value =
    let
        modifiers: Array.Array Int
        modifiers = (Array.fromList [-5, -5, -4, -4, -3, -3, -2, -2, -1, -1, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10 ])
    in
    Array.get value modifiers

raceToString: Race -> String
raceToString race =
    case race of
        Human -> "Human"
        Elf -> "Elf"
        Dwarf -> "Dwarf"
        NoRace -> ""

subRaceToString: SubRace -> String
subRaceToString subRace =
    case subRace of
        HillsDwarf -> "HillsDwarf"
        MountainsDwarf -> "MountainsDwarf"
        HighElf -> "HighElf"
        WoodElf -> "WoodElf"
        Drow -> "Drow"
        NoSubRace -> ""

stringToRace: String -> Race
stringToRace string =
    case string of
        "Human" -> Human
        "Elf" -> Elf
        "Dwarf" -> Dwarf
        _ -> NoRace

stringToSubRace: String -> SubRace
stringToSubRace string =
    case string of
        "HillsDwarf" -> HillsDwarf
        "MountainsDwarf" -> MountainsDwarf
        "HighElf" -> HighElf
        "WoodElf" -> WoodElf
        "Drow" -> Drow
        _ -> NoSubRace

getRaceBonus: Race -> Stats
getRaceBonus race =
    case race of
        Dwarf ->
            { strength = 0
            , constitution = 2
            , dexterity = 0
            , intelligence = 0
            , wisdom = 0
            , charisma = 0
            }
        Elf ->
            { strength = 0
            , constitution = 0
            , dexterity = 2
            , intelligence = 0
            , wisdom = 0
            , charisma = 0
            }
        Human ->
            { strength = 1
            , constitution = 1
            , dexterity = 1
            , intelligence = 1
            , wisdom = 1
            , charisma = 1
            }
        NoRace ->
            { strength = 0
            , constitution = 0
            , dexterity = 0
            , intelligence = 0
            , wisdom = 0
            , charisma = 0
            }

getSubRaceBonus: SubRace -> Stats
getSubRaceBonus subRace =
    case subRace of
        HillsDwarf ->
            { strength = 0
            , constitution = 0
            , dexterity = 0
            , intelligence = 0
            , wisdom = 1
            , charisma = 0
            }
        MountainsDwarf ->
            { strength = 2
            , constitution = 0
            , dexterity = 0
            , intelligence = 0
            , wisdom = 0
            , charisma = 0
            }
        Drow ->
            { strength = 0
            , constitution = 0
            , dexterity = 0
            , intelligence = 0
            , wisdom = 0
            , charisma = 1
            }
        WoodElf ->
            { strength = 0
            , constitution = 0
            , dexterity = 0
            , intelligence = 0
            , wisdom = 1
            , charisma = 0
            }
        HighElf ->
            { strength = 0
            , constitution = 0
            , dexterity = 0
            , intelligence = 1
            , wisdom = 0
            , charisma = 0
            }
        NoSubRace ->
            { strength = 0
            , constitution = 0
            , dexterity = 0
            , intelligence = 0
            , wisdom = 0
            , charisma = 0
            }

computeStatCost: Int -> Int
computeStatCost value =
    case value of
        8 ->
            0
        9 ->
            1
        10 ->
            2
        11 ->
            3
        12 ->
            4
        13 ->
            5
        14 ->
            7
        15 ->
            9
        _ ->
            0


main = 
    Browser.sandbox { init = init, update = update, view = view }
