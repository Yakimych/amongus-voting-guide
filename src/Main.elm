module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, input, span, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (checked, id, style, type_, value)
import Html.Events exposing (onClick, onInput)


initialModel : Model
initialModel =
    { numberOfPlayers = 10
    , numberOfImpostors = 2
    , lightsAreOn = True
    , expandReasons = False
    }


main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }


type DecisionType
    = MustVote
    | OkToVote
    | DoNotVote
    | NotApplicable


type alias VotingDecision =
    { decisionType : DecisionType, reason : String }


type alias Model =
    { numberOfPlayers : Int
    , numberOfImpostors : Int
    , lightsAreOn : Bool
    , expandReasons : Bool
    }


type Msg
    = IncrementPlayers
    | DecrementPlayers
    | IncrementImpostors
    | DecrementImpostors
    | SetNumberOfPlayers String
    | SetNumberOfImpostors String
    | FlipLights
    | FlipExpandReasons


getDecision : Int -> Int -> Bool -> VotingDecision
getDecision numberOfPlayers numberOfImpostors lightsAreOn =
    case ( numberOfPlayers, numberOfImpostors, lightsAreOn ) of
        ( 7, 2, True ) ->
            { decisionType = DoNotVote, reason = "Voting on 7 (unless absolutely sure) is very risky, since a wrong vote leads to a situation when a double-kill wins the game for the impostors." }

        ( 7, 2, False ) ->
            { decisionType = DoNotVote, reason = "With lights out crew might have 2 shots. If it is between 2 people (e.g. if they are accusing each other), it's ok to vote out one, fix lights, and vote out the other one. Lights being off does not allow impostors to sabotage. Keep in mind, however, that one impostor can mess with lights and not allow crewmates to fix them while the other one waits for the kill cooldown to go down and kill. So it might be a risky strategy anyway." }

        ( 6, 2, _ ) ->
            { decisionType = MustVote, reason = "Voting on 6 is pretty much a must, since impostors can sabotage and win with a double kill if played correctly." }

        ( 5, 2, True ) ->
            { decisionType = MustVote, reason = "Impostors only need 1 kill - they can easily sabotage and prevent crew from calling a meeting. Voting out someone randomly is better than skipping, since that would lead to a definite loss. Also crew should make sure that everyone votes the same person, otherwise both impostors can skip and tie the vote." }

        ( 5, 2, False ) ->
            { decisionType = MustVote, reason = "Same reasoning applies as when voting on 5 with lights on, but in this case crew can get more discussion time if necessary. Skip, fix lights, call a meeting, and continue the discussion is possible." }

        ( 4, 1, True ) ->
            { decisionType = DoNotVote, reason = "Same reasoning applies as when voting on 7 with 2 impostors. An incorrect vote would allow the impostor to sabotage and win." }

        ( 4, 1, False ) ->
            { decisionType = OkToVote, reason = "Same reasoning applies as when voting on 6 with 2 impostors. Crew has 2 shots, and it is less risky, since a single impostor is not able to both mess with the lights and kill. It is also ok to skip on 4." }

        ( 3, 1, True ) ->
            { decisionType = MustVote, reason = "Same reasoning applies as when voting on 5 with 2 impostors. An incorrect vote would allow the impostor to sabotage and win." }

        ( 3, 1, False ) ->
            { decisionType = OkToVote, reason = "Should generally vote, but the fact that lights are off, gives crew the opportunity to fix them and call a meeting - if more discussion time is necessary." }

        ( _, 2, _ ) ->
            if numberOfPlayers > 7 then
                { decisionType = OkToVote, reason = "With 2 impostors, it's ok to vote based off a minor suspicion. Even if a crewmate is voted off, impostors won't 'automatically' win with best play (as they would on 6 or 5 with lights on)." }

            else
                { decisionType = NotApplicable, reason = "" }

        ( _, 1, _ ) ->
            { decisionType = OkToVote, reason = "With 2 impostors, it's ok to vote based off a minor suspicion. Even if a crewmate is voted off, the impostor won't 'automatically' win with best play (as they would on 3 with lights on)." }

        ( _, _, _ ) ->
            { decisionType = OkToVote, reason = "TODO" }


decisionTypeToDiv votingDecision showReason =
    let
        reasonDiv =
            div [ style "font-size" "10px" ]
                [ text
                    (if showReason then
                        votingDecision.reason

                     else
                        ""
                    )
                ]
    in
    case votingDecision.decisionType of
        DoNotVote ->
            div [ style "background-color" "pink" ] [ text "Do NOT vote", reasonDiv ]

        OkToVote ->
            div [ style "background-color" "lightgray" ] [ text "OK to vote", reasonDiv ]

        MustVote ->
            div [ style "background-color" "lightgreen" ] [ text "Must Vote!", reasonDiv ]

        NotApplicable ->
            div [ style "background-color" "white" ] [ text "N/A", reasonDiv ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        IncrementPlayers ->
            { model | numberOfPlayers = model.numberOfPlayers + 1 }

        DecrementPlayers ->
            { model | numberOfPlayers = model.numberOfPlayers - 1 }

        IncrementImpostors ->
            { model | numberOfImpostors = model.numberOfImpostors + 1 }

        DecrementImpostors ->
            { model | numberOfImpostors = model.numberOfImpostors - 1 }

        SetNumberOfPlayers val ->
            { model | numberOfPlayers = String.toInt val |> Maybe.withDefault 0 }

        SetNumberOfImpostors val ->
            { model | numberOfImpostors = String.toInt val |> Maybe.withDefault 0 }

        FlipLights ->
            { model | lightsAreOn = not model.lightsAreOn }

        FlipExpandReasons ->
            { model | expandReasons = not model.expandReasons }


tableCellStyle =
    [ style "border" "1px solid black", style "padding" "5px" ]


highlightedTableCellStyle =
    [ style "border" "2px solid red", style "padding" "4px" ]


getTableCellStyle model forNumberOfPlayers forNumberOfImpostors forLights =
    if forNumberOfPlayers == model.numberOfPlayers && forNumberOfImpostors == model.numberOfImpostors && forLights == model.lightsAreOn then
        highlightedTableCellStyle

    else
        tableCellStyle


tableRowForPlayers model players =
    tr []
        [ td tableCellStyle [ text (String.fromInt players) ]
        , td (getTableCellStyle model players 2 True) [ decisionTypeToDiv (getDecision players 2 True) model.expandReasons ]
        , td (getTableCellStyle model players 1 True) [ decisionTypeToDiv (getDecision players 1 True) model.expandReasons ]
        , td (getTableCellStyle model players 2 False) [ decisionTypeToDiv (getDecision players 2 False) model.expandReasons ]
        , td (getTableCellStyle model players 1 False) [ decisionTypeToDiv (getDecision players 1 False) model.expandReasons ]
        ]


decisionTable : Model -> Html Msg
decisionTable model =
    div []
        [ table []
            [ thead []
                [ tr []
                    [ td tableCellStyle [ text "Players" ]
                    , td tableCellStyle [ text "2 impostors" ]
                    , td tableCellStyle [ text "1 impostor" ]
                    , td tableCellStyle [ text "2 impostors (off)" ]
                    , td tableCellStyle [ text "1 impostor (off)" ]
                    ]
                ]
            , tbody [] ([ 10, 9, 8, 7, 6, 5, 4, 3 ] |> List.map (tableRowForPlayers model))
            ]
        ]


view : Model -> Html Msg
view model =
    let
        decision =
            getDecision model.numberOfPlayers model.numberOfImpostors model.lightsAreOn
    in
    div []
        [ div []
            [ button [ onClick IncrementPlayers ] [ text "+" ]
            , input [ value (String.fromInt model.numberOfPlayers), onInput SetNumberOfPlayers ] []
            , button [ onClick DecrementPlayers ] [ text "-" ]
            ]
        , div []
            [ button [ onClick IncrementImpostors ] [ text "+" ]
            , input [ value (String.fromInt model.numberOfImpostors), onInput SetNumberOfImpostors ] []
            , button [ onClick DecrementImpostors ] [ text "-" ]
            ]
        , input [ type_ "checkbox", onClick FlipLights, checked model.lightsAreOn ] []
        , span [ id "test" ] [ text "Lights are on" ]
        , input [ type_ "checkbox", onClick FlipExpandReasons, checked model.expandReasons ] []
        , span [ id "test" ] [ text "Expand reasons" ]
        , decisionTable model
        , div [] [ text decision.reason ]
        ]
