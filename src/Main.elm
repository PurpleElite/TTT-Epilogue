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
    , currentChar : String
    , dialogueShow : Bool
    , dialogue : List String
    , stepCount : Int
    }


initialModel : Model
initialModel =
    { count = 0
    , pressedKeys = []
    , visiblePlayers = Set.fromList [ "Os", "Maela" ]
    , currentChar = ""
    , dialogueShow = False
    , dialogue = []
    , stepCount = 0
    }



-- UPDATE


type Msg
    = SetDialogue PartyMember
    | Remove String
    | DialogueStep Float
    | KeyMsg Keyboard.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetDialogue character ->
            ( { model
                | dialogue = String.split "\n" character.text
                , dialogueShow = True
                , currentChar = character.name
                , stepCount = 0
              }
            , Cmd.none
            )

        Remove name ->
            ( { model | visiblePlayers = Set.remove name model.visiblePlayers }, Cmd.none )

        DialogueStep time ->
            ( step model, Cmd.none )

        KeyMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }, Cmd.none )



-- FUNCTIONS


step model =
    let
        nextLine =
            model.stepCount > String.length (getCurrentLine model) && keysDown [ Enter ] model
    in
    { model
        | dialogueShow =
            List.length model.dialogue > 0
        , dialogue =
            if nextLine then
                List.tail model.dialogue |> Maybe.withDefault []

            else
                model.dialogue
        , stepCount =
            if nextLine then
                0

            else if keysDown [ Spacebar ] model then
                model.stepCount + 5

            else
                model.stepCount + 1
        , visiblePlayers =
            case model.dialogue of
                head :: rest -> model.visiblePlayers

                _ -> Set.remove model.currentChar model.visiblePlayers
      }


keysDown : List Key -> Model -> Bool
keysDown keys model =
    keys |> List.any (\a -> List.member a model.pressedKeys)


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


textToHtml : String -> Int -> Html msg
textToHtml dialogueLine stepCount =
    let
        stepsPerChar =
            2

        charsToShow =
            toFloat stepCount / stepsPerChar |> floor
    in
    String.slice 0 charsToShow dialogueLine |> Html.text


getCurrentLine : Model -> String
getCurrentLine model =
    List.head model.dialogue |> Maybe.withDefault ""


buttonDisable : Model -> Html.Attribute msg
buttonDisable model =
    Html.Attributes.disabled model.dialogueShow



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.dialogueShow then
            onAnimationFrameDelta DialogueStep

          else
            Sub.none
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
                        ]
                        [ text a.name ]
                )
                visibleCharacters

        dialogueBoxHtml =
            if model.dialogueShow then
                [ textToHtml (getCurrentLine model) model.stepCount ]

            else
                []
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
{-

        _     . _ _
  .  *    ` -*      \
| *-.               /.
|       *-,. _      `-\
 \             *-._    \
.*        /*-        * .\
|        |   `          |
|         `- /          /
 *-._                  ||
      *-._             *\
           *-._          |
                *-._     |
                     *-._*

            CHEESE GROMIT!
-}
