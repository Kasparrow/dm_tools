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
        { strength = 8
        , dexterity = 8
        , constitution = 8
        , intelligence = 8
        , wisdom = 8
        , charisma = 8
        }
    , race = NoRace
    , subRace = NoSubRace
    , class = NoClass
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
              , h3 [] [ text "Class" ]
              , viewClassInput
              , h3 [] [ text "Rolled stats" ]
              , div [ class "flex-row" ]
                    [ viewStatInput "STR" model.rolledStats.strength IncrementStrength DecrementStrength
                    , viewStatInput "DEX" model.rolledStats.dexterity IncrementDexterity DecrementDexterity
                    , viewStatInput "CON" model.rolledStats.constitution IncrementConstitution DecrementConstitution
                    , viewStatInput "INT" model.rolledStats.intelligence IncrementIntelligence DecrementIntelligence
                    , viewStatInput "WIS" model.rolledStats.wisdom IncrementWisdom DecrementWisdom
                    , viewStatInput "CHA" model.rolledStats.charisma IncrementCharisma DecrementCharisma
                    , viewRemainingPoints model.remainingPoints
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
        Warlock -> viewOption "Warloc"
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

viewRemainingPoints: Int -> Html Msg
viewRemainingPoints remainingPoints =
    div [ class "stat-reader" ]
        [ span [ class "stat-reader-title" ] [ text "POINTS" ]
        , div [ class "stat-reader-body" ]
              [ span [ class "stat-reader-value" ] [ text (String.fromInt remainingPoints) ]
              ]
        ]

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
            UpdateClass value ->
                { model | class = (stringToClass value) }

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
        Dragonborn ->
            { strength = 2
            , constitution = 0
            , dexterity = 0
            , intelligence = 0
            , wisdom = 0
            , charisma = 1
            }
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
        Gnome ->
            { strength = 0
            , constitution = 0
            , dexterity = 0
            , intelligence = 2
            , wisdom = 0
            , charisma = 0
            }
        HalfElf ->
            { strength = 0
            , constitution = 0
            , dexterity = 0
            , intelligence = 0
            , wisdom = 0
            , charisma = 2
            }
        Halfling ->
            { strength = 0
            , constitution = 0
            , dexterity = 2
            , intelligence = 0
            , wisdom = 0
            , charisma = 0
            }
        HalfOrc ->
            { strength = 2
            , constitution = 1
            , dexterity = 0
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
        Tiefling ->
            { strength = 0
            , constitution = 0
            , dexterity = 0
            , intelligence = 1
            , wisdom = 0
            , charisma = 2
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
        DeepGnome ->
            { strength = 0
            , constitution = 0
            , dexterity = 1
            , intelligence = 0
            , wisdom = 0
            , charisma = 0
            }
        RockGnome ->
            { strength = 0
            , constitution = 1
            , dexterity = 0
            , intelligence = 0
            , wisdom = 0
            , charisma = 0
            }
        LightfootHalfling ->
            { strength = 0
            , constitution = 0
            , dexterity = 0
            , intelligence = 0
            , wisdom = 0
            , charisma = 1
            }
        StoutHalfling ->
            { strength = 0
            , constitution = 1
            , dexterity = 0
            , intelligence = 0
            , wisdom = 0
            , charisma = 0
            }
        _ ->
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
