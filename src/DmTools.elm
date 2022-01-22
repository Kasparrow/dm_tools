module DmTools exposing (main)

import Html exposing (Html, div, h1, img, span, text, label, input, select, option, br, a, nav, p, footer, h3)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Array
import Browser

type Msg
    = UpdateRemainingPoints
    | UpdateRace String
    | UpdateSubRace String
    | UpdateClass String
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
        [ (Strength, 8)
        , (Dexterity, 8)
        , (Constitution, 8)
        , (Intelligence, 8)
        , (Wisdom, 8)
        , (Charisma, 8)
        ]
    , race = NoRace
    , subRace = NoSubRace
    , class = NoClass
    , remainingPoints = 27
    }

-- MODEL

type StatName
    = Strength
    | Dexterity
    | Constitution
    | Intelligence
    | Wisdom
    | Charisma

type alias Stat = (StatName, Int)
type alias Stats = List Stat

type Race 
    = Dragonborn
    | Dwarf
    | Elf
    | Gnome
    | HalfElf
    | Halfling
    | HalfOrc
    | Human
    | Tiefling
    | NoRace

type SubRace
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
    | Drow
    | HighElf
    | WoodElf
    | DeepGnome
    | RockGnome
    | LightfootHalfling
    | StoutHalfling
    | NoSubRace

enumRace = 
    [ NoRace
    , Dragonborn
    , Dwarf
    , Elf
    , Gnome
    , HalfElf
    , Halfling
    , HalfOrc
    , Human
    , Tiefling
    ]

type Class
    = Barbarian
    | Bard
    | Cleric
    | Druid
    | Fighter
    | Monk
    | Paladin
    | Ranger
    | Rogue
    | Sorcerer
    | Warlock
    | Wizard
    | NoClass

enumClass =
    [ NoClass
    , Barbarian
    , Cleric
    , Druid
    , Fighter
    , Monk
    , Paladin
    , Ranger
    , Rogue
    , Sorcerer
    , Warlock
    , Wizard
    ]

getSubRaces: Race -> List SubRace
getSubRaces race =
    case race of
        Dragonborn -> 
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
            ]
        Dwarf -> [ NoSubRace, HillsDwarf, MountainsDwarf ]
        Elf -> [ NoSubRace, Drow, WoodElf, HighElf ]
        Gnome -> [ NoSubRace, DeepGnome, RockGnome ]
        Halfling -> [ NoSubRace, LightfootHalfling, StoutHalfling ]
        _ -> [ NoSubRace ]

type alias Model = 
    { rolledStats: Stats
    , race: Race
    , subRace: SubRace
    , class: Class
    , remainingPoints: Int
    }

-- VIEW

view: Model -> Html Msg
view model =
    let
        classProficiencySaves = getClassProficiencySave model.class
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
              , h3 [] [ text "Class" ]
              , viewClassInput
              , h3 [] [ text "Rolled stats" ]
              , div [ class "flex-row" ]
                    [ viewStatInput "STR" (getStatValue model.rolledStats Strength) IncrementStrength DecrementStrength
                    , viewStatInput "DEX" (getStatValue model.rolledStats Dexterity) IncrementDexterity DecrementDexterity
                    , viewStatInput "CON" (getStatValue model.rolledStats Constitution) IncrementConstitution DecrementConstitution
                    , viewStatInput "INT" (getStatValue model.rolledStats Intelligence) IncrementIntelligence DecrementIntelligence
                    , viewStatInput "WIS" (getStatValue model.rolledStats Wisdom) IncrementWisdom DecrementWisdom
                    , viewStatInput "CHA" (getStatValue model.rolledStats Charisma) IncrementCharisma DecrementCharisma
                    , viewRemainingPoints model.remainingPoints
                    ]
              , br [] []
              , h3 [] [ text "Computed stats" ]
              , div [ class "flex-row" ]
                    [ viewStatReader Strength model classProficiencySaves
                    , viewStatReader Dexterity model classProficiencySaves
                    , viewStatReader Constitution model classProficiencySaves
                    , viewStatReader Intelligence model classProficiencySaves
                    , viewStatReader Wisdom model classProficiencySaves
                    , viewStatReader Charisma model classProficiencySaves
                    ]
              , viewCharacterBaseLife model
              ]
        , footer []
                 [ p [] [ text "/[A-Z]IKWAN/" ] ]
        ]

viewRaceInput =
    select [onInput UpdateRace] (List.map viewRaceOption enumRace)

viewRaceOption race =
    case race of
        Dragonborn -> viewOption "Dragonborn"
        Dwarf -> viewOption "Dwarf"
        Elf -> viewOption "Elf"
        Gnome -> viewOption "Gnome"
        HalfElf -> viewOption "Half-Elf"
        Halfling -> viewOption "Halfling"
        HalfOrc -> viewOption "Half-Orc"
        Human -> viewOption "Human"
        Tiefling -> viewOption "Tielfling"
        NoRace -> option [ value "" ] [ text "Select a race" ]

viewSubRaceInput: Race -> Html Msg
viewSubRaceInput currentRace =
    let 
        subRaces = getSubRaces currentRace
    in
    if List.length subRaces > 1 then
        select [onInput UpdateSubRace] (List.map viewSubRaceOption subRaces)
    else
       Html.text "" 

viewClassInput =
    select [ onInput UpdateClass ] (List.map viewClassOption enumClass)

viewClassOption class =
    case class of
        Barbarian -> viewOption "Barbarian"
        Bard -> viewOption "Bard"
        Cleric -> viewOption "Cleric"
        Druid -> viewOption "Druid"
        Fighter -> viewOption "Fighter"
        Monk -> viewOption "Monk"
        Paladin -> viewOption "Paladin"
        Ranger -> viewOption "Ranger"
        Rogue -> viewOption "Rogue"
        Sorcerer -> viewOption "Sorcerer"
        Warlock -> viewOption "Warlock"
        Wizard -> viewOption "Wizard"
        NoClass -> option [ value "" ] [ text "Select a class" ]

viewSubRaceOption: SubRace -> Html Msg
viewSubRaceOption subRace =
    case subRace of
        BlackDragonborn -> viewOption "Black Dragonborn"
        BlueDragonborn -> viewOption "Blue Dragonborn"
        BrassDragonborn -> viewOption "Brass Dragonborn"
        BronzeDragonborn -> viewOption "Bronze Dragonborn"
        CopperDragonborn -> viewOption "Bronze Dragonborn"
        GoldDragonborn -> viewOption "Gold Dragonborn"
        GreenDragonborn -> viewOption "Green Dragonborn"
        RedDragonborn -> viewOption "Red Dragonborn"
        SilverDragonborn -> viewOption "Silver Dragonborn"
        WhiteDragonborn -> viewOption "White Dragonborn"
        HillsDwarf -> viewOption "Hills Dwarf"
        MountainsDwarf -> viewOption "Mountains Dwarf"
        HighElf -> viewOption "High Elf"
        WoodElf -> viewOption "Wood Elf"
        Drow -> viewOption "Drow"
        DeepGnome -> viewOption "Deep Gnome"
        RockGnome -> viewOption "Rock Gnome"
        LightfootHalfling -> viewOption "Lightfoot Halfling"
        StoutHalfling -> viewOption "Stout Halfling"
        NoSubRace -> option [ value "" ] [ text "Select a subrace" ]

viewOption: String -> Html Msg
viewOption label =
    option [ value (stringToId label) ] [ text label ]

viewCharacterBaseLife: Model -> Html Msg
viewCharacterBaseLife model =
    if model.class /= NoClass then
        span [] [ text ("Base life " ++ (String.fromInt (getCharacterBaseLife model))) ]
    else
        Html.text ""

stringToId: String -> String
stringToId string =
    String.replace "-" "" (String.replace " " "" string)

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

viewStatReader: StatName -> Model -> List StatName -> Html Msg
viewStatReader statName model proficiencySaves =
    let
        hasProficiencySave = List.member statName proficiencySaves
        value = getFinalStatValue model statName
    in
    div [ class "stat-reader" ]
        [ span [ class "stat-reader-title" ] [ text (statNameToString statName) ]
        , div [ class "stat-reader-body" ]
              [ span [ class "stat-reader-value" ] [ text (String.fromInt value) ]
              , div [ class "stat-reader-bonus" ]
                    [ span [] [ text (printWithSign (computeModifier value)) ]
                    , span [] [ text (if hasProficiencySave then "M" else "")]
                    ]
              ]
        ]

viewRemainingPoints: Int -> Html Msg
viewRemainingPoints remainingPoints =
    div [ class "stat-reader" ]
        [ span [ class "stat-reader-title" ] [ text "POINTS" ]
        , div [ class "stat-reader-body" ]
              [ span [ class "stat-reader-value" ] [ text (String.fromInt remainingPoints) ]
              ]
        ]

-- UPDATE

incrementStat: Stats -> StatName -> Stats
incrementStat stats statName =
    List.map (\stat -> if Tuple.first stat == statName then (statName, (Tuple.second stat + 1)) else stat) stats

decrementStat: Stats -> StatName -> Stats
decrementStat stats statName =
    List.map (\stat -> if Tuple.first stat == statName then (statName, (Tuple.second stat - 1)) else stat) stats

update: Msg -> Model -> Model
update msg ({rolledStats} as model) =
        case msg of
            IncrementStrength -> 
                update UpdateRemainingPoints { model | rolledStats = (incrementStat rolledStats Strength) }
            DecrementStrength -> 
                update UpdateRemainingPoints { model | rolledStats = (decrementStat rolledStats Strength) }
            IncrementDexterity -> 
                update UpdateRemainingPoints { model | rolledStats = (incrementStat rolledStats Dexterity) }
            DecrementDexterity -> 
                update UpdateRemainingPoints { model | rolledStats = (decrementStat rolledStats Dexterity) }
            IncrementConstitution -> 
                update UpdateRemainingPoints { model | rolledStats = (incrementStat rolledStats Constitution) }
            DecrementConstitution ->
                update UpdateRemainingPoints { model | rolledStats = (decrementStat rolledStats Constitution) }
            IncrementIntelligence -> 
                update UpdateRemainingPoints { model | rolledStats = (incrementStat rolledStats Intelligence) }
            DecrementIntelligence -> 
                update UpdateRemainingPoints { model | rolledStats = (decrementStat rolledStats Intelligence) }
            IncrementWisdom -> 
                update UpdateRemainingPoints { model | rolledStats = (incrementStat rolledStats Wisdom) }
            DecrementWisdom -> 
                update UpdateRemainingPoints { model | rolledStats = (decrementStat rolledStats Wisdom) }
            IncrementCharisma -> 
                update UpdateRemainingPoints { model | rolledStats = (incrementStat rolledStats Charisma) }
            DecrementCharisma -> 
                update UpdateRemainingPoints { model | rolledStats = (decrementStat rolledStats Charisma) }

            UpdateRemainingPoints ->
                { model | remainingPoints = (27 - (computeRemainingPoints model.rolledStats)) }
            UpdateRace value ->
                { model | race = (stringToRace value), subRace = NoSubRace }
            UpdateSubRace value ->
                { model | subRace = (stringToSubRace value ) }
            UpdateClass value ->
                { model | class = (stringToClass value) }

-- HELPERS

getFinalStatValue: Model -> StatName -> Int
getFinalStatValue model statName =
    let
        rolledStat = getStatValue model.rolledStats statName
        raceBonusStat = getStatValue (getRaceBonus model.race) statName
        subRaceBonusStat = getStatValue (getSubRaceBonus model.subRace) statName
    in
    rolledStat + raceBonusStat + subRaceBonusStat

getStatValue: Stats -> StatName -> Int
getStatValue stats statName =
    let
        selectedStats = List.filter (\stat -> Tuple.first stat == statName) stats
        tail = List.head selectedStats
    in
    case tail of
        Just stat -> Tuple.second stat
        Nothing -> 0

printWithSign: Maybe Int -> String
printWithSign value =
    case value of
        Just int ->
            if int >= 0 then
                "+" ++ (String.fromInt int)
            else
                String.fromInt int
        Nothing ->
            "?"


computeRemainingPoints: Stats -> Int
computeRemainingPoints stats =
    List.sum (List.map (\stat -> computeStatCost (Tuple.second stat)) stats)

computeModifier: Int -> Maybe Int
computeModifier value =
    let
        modifiers: Array.Array Int
        modifiers = (Array.fromList [-5, -5, -4, -4, -3, -3, -2, -2, -1, -1, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10 ])
    in
    Array.get value modifiers

statNameToString: StatName -> String
statNameToString statName =
    case statName of
        Strength -> "STR"
        Dexterity -> "DEX"
        Constitution -> "CON"
        Intelligence -> "INT"
        Wisdom -> "WIS"
        Charisma -> "CHA"

raceToString: Race -> String
raceToString race =
    case race of
        Dragonborn -> "Dragonborn"
        Dwarf -> "Dwarf"
        Elf -> "Elf"
        Gnome -> "Gnome"
        HalfElf -> "HalfElf"
        Halfling -> "Halfling"
        HalfOrc -> "HalfOrc"
        Human -> "Human"
        Tiefling -> "Tiefling"
        NoRace -> ""

subRaceToString: SubRace -> String
subRaceToString subRace =
    case subRace of
        BlackDragonborn -> "BlackDragonborn"
        BlueDragonborn -> "BlueDragonborn"
        BrassDragonborn -> "BrassDragonborn"
        BronzeDragonborn -> "BronzeDragonborn"
        CopperDragonborn -> "CopperDragonborn"
        GoldDragonborn -> "GoldDragonborn"
        GreenDragonborn -> "GreenDragonborn"
        RedDragonborn -> "RedDragonborn"
        SilverDragonborn -> "SilverDragonborn"
        WhiteDragonborn -> "WhiteDragonborn"
        HillsDwarf -> "HillsDwarf"
        MountainsDwarf -> "MountainsDwarf"
        Drow -> "Drow"
        HighElf -> "HighElf"
        WoodElf -> "WoodElf"
        DeepGnome -> "DeepGnome"
        RockGnome -> "RockGnome"
        LightfootHalfling -> "LightfootHalfling"
        StoutHalfling -> "StoutHalfling"
        NoSubRace -> ""

stringToRace: String -> Race
stringToRace string =
    case string of
        "Dragonborn" -> Dragonborn
        "Dwarf" -> Dwarf
        "Elf" -> Elf
        "Gnome" -> Gnome
        "HalfElf" -> HalfElf
        "Halfling" -> Halfling
        "HalfOrc" -> HalfOrc
        "Human" -> Human
        "Tiefling" -> Tiefling
        _ -> NoRace

stringToSubRace: String -> SubRace
stringToSubRace string =
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

stringToClass: String -> Class
stringToClass string =
    case string of
        "Barbarian" -> Barbarian
        "Bard" -> Bard
        "Cleric" -> Cleric
        "Druid" -> Druid
        "Fighter" -> Fighter
        "Monk" -> Monk
        "Paladin" -> Paladin
        "Ranger" -> Ranger
        "Rogue" -> Rogue
        "Sorcerer" -> Rogue
        "Warlock" -> Warlock
        "Wizard" -> Wizard
        _ -> NoClass

getRaceBonus: Race -> Stats
getRaceBonus race =
    case race of
        Dragonborn -> [ (Strength, 2), (Charisma, 1) ]
        Dwarf -> [ (Constitution, 2) ]
        Elf -> [ (Dexterity, 2)]
        Gnome -> [ (Intelligence, 2) ]
        HalfElf -> [ (Charisma, 2) ]
        Halfling -> [ (Dexterity, 2) ]
        HalfOrc -> [ (Strength, 2), (Constitution, 1) ]
        Human -> 
            [ (Strength, 1)
            , (Dexterity, 1)
            , (Constitution, 1)
            , (Intelligence, 1)
            , (Wisdom, 1)
            , (Charisma, 1)
            ]
        Tiefling -> [ (Intelligence, 1), (Charisma, 2) ]
        NoRace -> []

getSubRaceBonus: SubRace -> Stats
getSubRaceBonus subRace =
    case subRace of
        HillsDwarf -> [ (Wisdom, 1) ]
        MountainsDwarf -> [ (Strength, 2) ]
        Drow -> [ (Charisma, 1) ]
        WoodElf -> [ (Wisdom, 1) ]
        HighElf -> [ (Intelligence , 1) ]
        DeepGnome -> [ (Dexterity, 1) ]
        RockGnome -> [ (Constitution, 1) ]
        LightfootHalfling -> [ (Charisma, 1) ]
        StoutHalfling -> [ (Constitution, 1) ]
        _ -> []

getClassProficiencySave: Class -> List StatName
getClassProficiencySave class =
    case class of
        Barbarian -> [ Strength, Constitution ]
        Bard -> [ Dexterity, Charisma ]
        Cleric -> [ Wisdom, Charisma ]
        Druid -> [ Intelligence, Wisdom ]
        Fighter -> [ Strength, Constitution ]
        Monk -> [ Strength, Dexterity ]
        Paladin -> [ Wisdom, Charisma ]
        Ranger -> [ Strength, Dexterity ]
        Rogue -> [ Dexterity, Intelligence ]
        Sorcerer -> [ Constitution, Charisma ]
        Warlock -> [ Wisdom, Charisma ]
        Wizard -> [ Intelligence, Wisdom ]
        NoClass -> []

getCharacterBaseLife: Model -> Int
getCharacterBaseLife model =
    let
        constitutionModifier = computeModifier (getFinalStatValue model Constitution)
    in

    case constitutionModifier of
        Just value ->
            case model.class of
                Barbarian -> 12 + value
                Bard -> 8 + value
                Cleric -> 8 + value
                Druid -> 8 + value
                Fighter -> 10 + value
                Monk -> 8 + value
                Paladin -> 10 + value
                Ranger -> 10 + value
                Rogue -> 8 + value
                Sorcerer -> 8 + value
                Warlock -> 6 + value
                Wizard -> 6 + value
                NoClass -> 0 + value
        Nothing -> 0

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
