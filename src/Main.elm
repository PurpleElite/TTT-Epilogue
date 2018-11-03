module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Keyboard exposing (Key(..))
import Set exposing (Set)



-- MODEL


-- type alias Model =
--     { count : Int
--     , pressedKeys : List Key
--     , visiblePlayers : Set String
--     , currentChar : String
--     , dialogueShow : Bool
--     , dialogue : List String
--     , stepCount : Int
--     }

-- "Make impossible states impossible" - Richard Feldman
type alias Model =
    { count : Int
    , pressedKeys : List Key
    , visiblePlayers : Set String
    , dialogueState : DialogueState
    , stepCount : Int
    , fadeOutStart : Maybe (String, Int)
    }

type DialogueState
    = NoDialogue
    | Dialogue DialogueData

type alias DialogueData =
    { currentChar : String
    , dialogue : List String
    , dialogueStart : Int
    }

initialModel : Model
initialModel =
    { count = 0
    , pressedKeys = []
    , visiblePlayers = Set.fromList [ "Os", "Maela" ]
    , dialogueState = NoDialogue
    , stepCount = 0
    , fadeOutStart = Nothing
    }



-- UPDATE


type Msg
    = SetDialogue PartyMember
    | Step Float
    | KeyMsg Keyboard.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetDialogue character ->
            ( { model
                | dialogueState =
                    { currentChar = character.name
                    , dialogue = String.split "\n" character.text
                    , dialogueStart = model.stepCount
                    }
                    |> Dialogue
              }
            , Cmd.none
            )

        Step time ->
            ( step model, Cmd.none )

        KeyMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }, Cmd.none )



-- FUNCTIONS


step model =
    let
        newDialogueState =
            case model.dialogueState of
                Dialogue state ->
                    let
                        nextLine =
                            (toFloat (timeElapsed model) / toFloat stepsPerChar |> floor) > String.length (getCurrentLine model) &&
                            keysDown [ Enter, Character "z" ] model
                    in
                    if nextLine && List.length state.dialogue == 0 then
                        NoDialogue
                    else
                        { state
                            | dialogue =
                                if nextLine then
                                    List.tail state.dialogue |> Maybe.withDefault []

                                else
                                    state.dialogue
                            , dialogueStart =
                              if nextLine then
                                  model.stepCount

                              else
                                  state.dialogueStart
                        }
                        |> Dialogue
                NoDialogue -> NoDialogue

        fadeDone startStep = model.stepCount - startStep > fadeDuration
    in
    { model
        | stepCount =
            if keysDown [ Spacebar, Character "x" ] model then
                model.stepCount + 5

            else
                model.stepCount + 1
        , dialogueState = newDialogueState
        , fadeOutStart =
            case (model.dialogueState, model.fadeOutStart) of
                (Dialogue state, a) ->
                    case state.dialogue of
                        head :: rest -> Nothing

                        _ -> Just (state.currentChar, model.stepCount)

                (NoDialogue, Just (character, startStep)) ->
                    if fadeDone startStep then
                        Nothing

                    else
                        model.fadeOutStart

                _ ->
                    model.fadeOutStart
        , visiblePlayers =
              case model.fadeOutStart of
                  Just ( character, startStep ) ->
                      if fadeDone startStep then
                          Set.remove character model.visiblePlayers

                      else
                          model.visiblePlayers
                  Nothing ->
                      model.visiblePlayers
      }


keysDown : List Key -> Model -> Bool
keysDown keys model =
    keys |> List.any (\a -> List.member a model.pressedKeys) |> Debug.log "keysDown"


type alias Point =
    { x : Int, y : Int }


type alias PartyMember =
    { name : String, pos : Point, text : String }


characters : List PartyMember
characters =
    [ { name = "Os", pos = { x = 14, y = 364 }, text = "Test string for Os! Test string for Os! Test string for Os! Test string for Os! Test string for Os!\nA second line of test string for Os! Wow! Incredible!" }
    , { name = "Maela", pos = { x = 14, y = 264 }, text = "Test string for Maela!" }
    ]


px : Int -> String
px value =
    String.fromInt value ++ "px"


timeElapsed : Model -> Int
timeElapsed model =
    case model.dialogueState of
        NoDialogue ->
            0

        Dialogue dialogueData ->
            model.stepCount - dialogueData.dialogueStart |> Debug.log "Dialogue Steps Elapsed"


stepsPerChar = 2

textToHtml : String -> Int -> Html msg
textToHtml dialogueLine stepCount =
    let
        charsToShow =
            toFloat stepCount / stepsPerChar |> floor
    in
    String.slice 0 charsToShow dialogueLine |> Html.text


getCurrentLine : Model -> String
getCurrentLine model =
    case model.dialogueState of
        NoDialogue ->
            ""

        Dialogue dialogueData ->
            List.head dialogueData.dialogue |> Maybe.withDefault ""


buttonDisable : Model -> Html.Attribute msg
buttonDisable model =
    Html.Attributes.disabled (model.dialogueState /= NoDialogue || model.fadeOutStart /= Nothing)


fadeDuration = 100


buttonOpacity : Model -> String -> Html.Attribute msg
buttonOpacity model buttonName =
    case model.fadeOutStart of
        Just ( character, startStep ) ->
            if buttonName == character then

                String.fromFloat ( 1 - toFloat ( model.stepCount - startStep ) / fadeDuration ) |> Html.Attributes.style "opacity"
            else
                Html.Attributes.style "opacity" "1"

        Nothing ->
            Html.Attributes.style "opacity" "1"






-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Step
        , Sub.map KeyMsg Keyboard.subscriptions
        ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        visibleCharacters =
            List.filter (\a -> Set.member a.name model.visiblePlayers) characters

        visibleCharacterHtml =
            List.map
                (\a ->
                    button
                        [ onClick (SetDialogue a)
                        , style "left" (px a.pos.x)
                        , style "top" (px a.pos.y)
                        , style "position" "relative"
                        , buttonDisable model
                        , buttonOpacity model a.name
                        ]
                        [ text a.name ]
                )
                visibleCharacters

        dialogueBoxHtml =
            if model.dialogueState == NoDialogue then
                []

            else
                [ textToHtml (getCurrentLine model) (timeElapsed model) ]
    in
    div [ style "position" "relative" ]
        (visibleCharacterHtml
            ++ dialogueBoxHtml
        )



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
