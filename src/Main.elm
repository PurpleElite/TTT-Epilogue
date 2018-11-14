module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Keyboard exposing (Key(..))
import Set exposing (Set)



-- MODEL

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
    , visiblePlayers = Set.fromList [ "Strawberry", "Valtyra", "Maela", "Newyn", "Arnulf", "Luxara", "Zayn", "Knifery", "Aveline", "Yannick", "Os", "Mask", "Samara" ]
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
    [ {name = "Strawberry", pos = {x = 77 , y = 25 }, text = ""}
    , {name = "Valtyra", pos = {x = 912 , y = 177 }, text = ""}
    , {name = "Maela", pos = {x = 551 , y = 31 }, text = "The first thing on Maela’s list was to marry Valtyra. That being done, she set herself to the task of becoming infamous. Sure, she had been the most wanted person on the planet, but that was thinking too small. She wanted even the gods to notice her!\nFortunately for her, they noticed without her actually doing anything. One minute there she was, and the next there were a bunch of gods descending upon her, demanding her wisdom. How had she survived in the mortal world, with a mortal form, for so long, they wondered? They were on the run from a creature of ultimate darkness, a genocider of the gods of the Immortal Realms, and needed to live in the mortal world for the foreseeable future. Maela shrugged. Possess mortals, she guessed?\nThe gods went ahead and did that, and Maela marked that off her list of things to do. She went ahead and had a house built in the Realm of Eternal Fire, assuming that Valtyra would agree to move there when her ventures with the Holy Order inevitably failed. They did, and the two of them lived happily forever after in Hell, except for those times that Valtyra’s fallen sister visited."}
    , {name = "Newyn", pos = {x = 1294 , y = 126 }, text = ""}
    , {name = "Arnulf", pos = {x = 226 , y = 355 }, text = ""}
    , {name = "Luxara", pos = {x = 1542 , y = 108 }, text = ""}
    , {name = "Zayn", pos = {x = 0 , y = 315 }, text = ""}
    , {name = "Knifery", pos = {x = 55 , y = 510 }, text = ""}
    , {name = "Aveline", pos = {x = 360 , y = 554 }, text = ""}
    , {name = "Yannick", pos = {x = 1276 , y = 533 }, text = ""}
    , { name = "Os", pos = { x = 808, y = 308 }, text = "Test string for Os! Test string for Os! Test string for Os! Test string for Os! Test string for Os!\nA second line of test string for Os! Wow! Incredible!" }
    , {name = "Mask", pos = {x = 1048 , y = 283 }, text = ""}
    , {name = "Samara", pos = {x = 586 , y = 463 }, text = ""}
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
                        , Html.Attributes.class a.name
                        , style "left" (px a.pos.x)
                        , style "top" (px a.pos.y)
                        , style "position" "fixed"
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
                [div
                    [ style "position" "fixed"
                    , style "left" (px 68)
                    , style "top" (px 699)
                    ]
                ( [ Html.img [Html.Attributes.src "/public/images/dialoguebox.png"] [] ]
                ++ [div
                    [ style "position" "absolute"
                    , style "top" "60px"
                    , style "left" "80px"
                    , style "width" "1620px"
                    , style "font-size" "300%"
                    --, style "transform" "translate(-50%, -50%)"
                    ]
                        [textToHtml (getCurrentLine model) (timeElapsed model) ]
                    ]
                )]
    in
    div [ style "position" "fixed" ]
        ([ Html.img [Html.Attributes.src "/public/images/0campfire.png"] [] ]
            ++ visibleCharacterHtml
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
