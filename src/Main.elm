port module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Encode as Enc exposing (object)
import Keyboard exposing (Key(..))
import Set exposing (Set)


port portOut : Enc.Value -> Cmd msg


type PortOutMsg
    = PlaySound { soundName : String, loop : Bool }
    | StopSound { soundName : String }


encodePortOutMsg : PortOutMsg -> Enc.Value
encodePortOutMsg a =
    case a of
        PlaySound a1 ->
            object
                [ ( "Constructor", Enc.string "PlaySound" )
                , ( "A1", encodeRecord_soundName_String_loop_Bool_ a1 )
                ]

        StopSound a1 ->
            object
                [ ( "Constructor", Enc.string "StopSound" )
                , ( "A1", encodeRecord_soundName_String_ a1 )
                ]


encodeRecord_soundName_String_ : { a | soundName : String } -> Enc.Value
encodeRecord_soundName_String_ a =
    object
        [ ( "soundName", Enc.string a.soundName )
        ]


encodeRecord_soundName_String_loop_Bool_ :
    { a | loop : Bool, soundName : String }
    -> Enc.Value
encodeRecord_soundName_String_loop_Bool_ a =
    object
        [ ( "soundName", Enc.string a.soundName )
        , ( "loop", Enc.bool a.loop )
        ]


playSound : String -> Bool -> Cmd msg
playSound soundName loop =
    PlaySound { soundName = soundName, loop = loop }
        |> encodePortOutMsg
        |> portOut


stopSound : String -> Cmd msg
stopSound soundName =
    StopSound { soundName = soundName }
        |> encodePortOutMsg
        |> portOut



-- MODEL


type alias Model =
    { count : Int
    , pressedKeys : List Key
    , visiblePlayers : Set String
    , dialogueState : DialogueState
    , stepCount : Int
    , fadeOutStart : Maybe ( String, Int )
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
    | DialogueNext


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
            , playSound ("/music/" ++ character.name ++ ".mp3") False
            )

        Step time ->
            ( step model
            , case model.fadeOutStart of
                Just ( character, startStep ) ->
                    if model.stepCount - startStep > fadeDuration then
                        Cmd.none

                    else
                        stopSound ("/music/" ++ character ++ ".mp3")

                _ ->
                    Cmd.none
            )

        KeyMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }, Cmd.none )

        DialogueNext ->
            ( { model
                | dialogueState =
                    case model.dialogueState of
                        Dialogue state ->
                            if List.length state.dialogue <= 1 then
                                NoDialogue

                            else
                                { state
                                    | dialogue =
                                        List.tail state.dialogue |> Maybe.withDefault []
                                    , dialogueStart =
                                        model.stepCount
                                }
                                    |> Dialogue

                        NoDialogue ->
                            NoDialogue
                , fadeOutStart =
                    case ( model.dialogueState, model.fadeOutStart ) of
                        ( Dialogue state, a ) ->
                            case state.dialogue of
                                head :: member :: rest ->
                                    Nothing

                                _ ->
                                    Just ( state.currentChar, model.stepCount )

                        ( NoDialogue, Just ( character, startStep ) ) ->
                            if model.stepCount - startStep > fadeDuration then
                                Nothing

                            else
                                model.fadeOutStart

                        _ ->
                            model.fadeOutStart
              }
            , Cmd.none
            )



-- FUNCTIONS


step model =
    let
        newDialogueState =
            case model.dialogueState of
                Dialogue state ->
                    let
                        nextLine =
                            (toFloat (timeElapsed model) / toFloat stepsPerChar |> floor)
                                > String.length (getCurrentLine model)
                                && keysDown [ Enter, Character "z" ] model
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

                NoDialogue ->
                    NoDialogue

        fadeDone startStep =
            model.stepCount - startStep > fadeDuration
    in
    { model
        | stepCount =
            if keysDown [ Spacebar, Character "x" ] model then
                model.stepCount + 5

            else
                model.stepCount + 1
        , dialogueState = newDialogueState
        , fadeOutStart =
            case ( model.dialogueState, model.fadeOutStart ) of
                ( Dialogue state, a ) ->
                    case state.dialogue of
                        head :: rest ->
                            Nothing

                        _ ->
                            Just ( state.currentChar, model.stepCount )

                ( NoDialogue, Just ( character, startStep ) ) ->
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
    keys |> List.any (\a -> List.member a model.pressedKeys)


type alias Point =
    { x : Int, y : Int }


type alias PartyMember =
    { name : String, pos : Point, text : String }


characters : List PartyMember
characters =
    [ { name = "Strawberry", pos = { x = 77, y = 25 }, text = "After the campfire, Strawberry’s bones were tossed into a shallow hole and the party said a couple words each before dispersing. It was the ceremony they figured she would have wanted, being a cat of almost no words and ultimately very little participation in the group’s past adventures.\n Some time later, Strawberry found herself awake and alone in that same grave, now a zombified version of herself. Confused as to what the deal was, she headed for the city to appraise herself of what had happened during her death. What she found was a dark new world. Apparently a necromancer had cast a spell to raise all the deceased animals for miles and ordered them all to a single location, and now he had been selling them off like slaves. This would not stand.\nStrawberry, as the only free zombie animal in the city, led a massive revolt. She ordered the zombie pets to cast off the shackles of oppression and claim freedom for themselves, which they did after being promised that they could still live with their owners and maintain their usual lives, just under the pretense of not technically being “owned.”\nContent in the success of her revolution, Strawberry retired to a local vet’s office, where she stayed for the rest of her (after)life." }
    , { name = "Valtyra", pos = { x = 912, y = 177 }, text = "The first thing on Valtyra’s list was to marry Maela.\nThat being done, she set herself to the task of reforming the Holy Order of Paladins. To this end she campaigned to be elected Grand Paladin. Her campaign was doomed from the start, not just because the incumbent sabotaged her, but because her marriage to the host for a god of pure chaotic evil didn’t make her very popular in the order.\n All of a sudden, interest in her magically surged overnight. Quite literally, one minute the order hated her, the next they loved her. Valtyra didn’t question the change, happily accepting the 90-10 vote split that made her the new Grand Paladin.\n As Grand Paladin she set to implementing sweeping reforms across the whole of the Forgotten Realms. Among her positive social changes, such as implementing implicit progressive values into protected law, she also implemented many programs designed to aid the poor.\n Chief among these was a sweeping rent reform designed to make apartments and tenements on the Sword Coast more affordable. Some unintended side effects aside, Valtyra felt content that she was helping people.\nThen, a couple months later, opinion on her within the order abruptly shifted again and she was voted out, 10-90, against a candidate who had no qualifications whatsoever, who let the corrupt paladins more-or-less take back over.\nSick of the order and sick of trying to fix problems that weren’t actually hers, she moved to the Realm of Eternal Fire, where Maela had set up a house for the two of them. She lived happily forever after, except for those times that her fallen sister visited." }
    , { name = "Maela", pos = { x = 551, y = 31 }, text = "The first thing on Maela’s list was to marry Valtyra.\nThat being done, she set herself to the task of spreading chaos across the Forgotten Realms. Now that Beshaba was her soul’s roommate, she had a quota to meet. The thought of causing minor chaos day after day was tiring, so Maela decided she would rather be responsible for a single chaotic action that would ripple across the realm for eons. But what?\nLuckily for her, one fell into her lap. A veritable army of Gods appeared before her one day, panicked and tired. They were on the run from a dark genocider, a being of pure power who was killing off the Gods in a mighty battle, and needed a way to blend in in the realm of mortals. How did Beshaba do it, they wondered?\nMaela’s eyes lit up - imagine, a whole population of Gods living among mortals and infinite issues with their power and whim! She told them all how to possess mortals and then lied as much as she could about what Forgotten Realms customs were like. Half of her lies were just borne out of her own misunderstands, but…\nThe gods went ahead assimilated into mortal culture in the most awful way possible, and Maela marked “ruin the Forgotten Realms forever” off her to-do list. It was time to retire.\nShe went ahead and had a house built in the Realm of Eternal Fire, assuming that Valtyra would agree to move there when her ventures with the Holy Order inevitably failed. They did, and the two of them lived happily forever after, except for those times that Valtyra’s fallen sister visited." }
    , { name = "Newyn", pos = { x = 1294, y = 126 }, text = "After the campfire, Newyn decided to try to fix his time curse. Of course he ultimately did not succeed, because if he did succeed, then his future self, who was around long long after the campfire, would not be able to be present for several of the stories told during it. But regardless, he made an attempt.\nThe attempt involved a powerful ritual that would alter reality itself around him, warping the fabric of space-time in tiny, nearly imperceptible ways and then fixing them. The specifics beyond this are too complicated for us to comprehend. The ritual was mostly a success, but it had two side effects.\n The first was that it caused a severe and sudden change in the results of a single election in the paladin city of Tristios. The other was that it sent Newyn spiralling through the ancient dimensions beyond the Forgotten Realms, lost and tangled in ethereal passageways beyond the ken of mortal beings, where time had no meaning.\n  Luckily for Newyn, an old friend happened to be passing by and offered to help send Newyn back to his realm, but not before inquiring about what brought Newyn there. Newyn’s future-self took over at this moment, fervently warning the friend about a horrific tragedy that was soon to befall the Forgotten Realms - “the death of a God, by a single killer: Bahamut” - which was about the worst possible way to phrase that warning.\nAfter imparting this dark information, the old friend helped to reverse Newyn’s spell and the time wizard was sent hurtling back to the Forgotten Realms, where he arrived in the same place he had departed, a few months later than he had cast the ritual. On his return, the result of another election in the paladin city of Tristios was radically altered.\nAs for Newyn’s future from there -- well, we’ve already seen it, haven’t we?\nHe did go on to make bank betting on horse races and sportsball games, though." }
    , { name = "Arnulf", pos = { x = 226, y = 355 }, text = "Arnulf went about applying himself to menial-yet-fulfilling jobs after the campfire. His physical prowess made him great for manual labor, but he lacked the professional aura for any position of real prestige in the cities of tall folk.\nThen, one day, a major company announced it was opening its doors to evil and chaotic employees, no questions asked. Arnulf figured he was chaotic enough to count and signed right up.\nThe company was a perfect fit and Arnulf ascended the ranks quickly with his amiable personality, natural leadership skills, and powerful muscles. The other employees were supernaturally talented, but even that didn’t stop Arnulf’s string of promotions.\n Eventually he started to notice that the company had changed from within: most of the chaotic and evil employees now used the company’s funds to traffic in drugs rather than actually do any work. Arnulf tried to recall a time he’d been in a similar situation, during the gnome trials, and came up with two pieces of advice his friends had given him: give back to the community, and join in the drug trafficking.\n He decided to combine the two, promising to join the cartel only if the evil workers would deposit some of their revenue back into the company so that the company could continue to profit.\n This proved to be a great compromise for the drug cartel, since shortly after joining, Arnulf was summoned back to his homeland. King Tarragon had fallen and he was to be the new Highland Gnome King. As king it would be a disaster if he was tied to a drug ring, so his first act in office was to flagrantly abuse his power to set protections around his former company, preventing the law from snooping around too much.\n Outside of that asterisk, Arnulf went on to be a respected and beloved king, adored beyond even his predecessor as the greatest gnome ruler of all time." }
    , { name = "Luxara", pos = { x = 1542, y = 108 }, text = "After departing from the others, Luxara was made an offer they couldn’t refuse - membership in a new cult! The Emmelem Cult, or so the spokeswoman who reached Luxara claimed, had heard of their prowess with cults and wanted to give them a special deal.\nFor only 100,000 gold coins, Luxara could join the cult at the rank of Diamond Initiate. Sure, the price might have *seemed* steep, but the Emmelem spokeswoman promised that really it would help Luxara *make* money, because they would be allowed to recruit members into the cult for the same price and keep half the money. What a deal!\n Luxara immediately accepted, of course, because who could pass that up?\nThey spent the next few years in abject poverty, turning to increasingly desperate measures to get by, including some dangerous heists and the sale of more than a couple legendary dark artifacts. The cult offered no assistance, adventuring was difficult alone, and Luxara didn’t want to lean on their friends for assistance.\n Right around when Luxara was ready to die alone in an alley somewhere, a crowd approached them.\nThey were gods possessing mortal bodies - a new thing they were trying out - and they were looking for assistance, perhaps an cult-like organization that could assist them. They had many - *many* - friends in the same situation, and nearly limitless funds. Did Luxara know anyone with cult experience who could help them out?\nLuxara retired as the richest being in the Forgotten Realms, with the leading rank of Ultimate Gold Lich-Possessed-Diamond in the Emmelem Cult." }
    , { name = "Zayn", pos = { x = 0, y = 315 }, text = "After the party went their separate ways, Zayn did some soul searching. He didn’t actually want to rule the world as a God-Emperor of Darkness, he realized, because that had a tendency to attract adventurers who wanted to murder you. No, he decided, it was best to go back home and run the family business.\nHe went back home and took over the business, but found it super boring. That was when an idea hit him: what if he opened employment to chaotic and evil persons? He had experience working with them, and it was an untapped labor market!\nHe sent out news that the Jameson business was hiring, and thousands of new employees flocked to him - many of them so talented that they might as well have been Gods. Business was lucrative - in fact, the Jameson family had never had so much profit before.\nThis ended up being because some of the employees were running the biggest black market drug scheme that the Sword Coast had ever seen, using funds from Zayn’s business, and were turning some of that profit back into the company.\nJust Zayn’s luck, a series of laws passed by the new Grand Paladin of the Holy Order, a former police chief who was incredibly strict on crime, made employers guilty of any crimes committed by those in their employ. This made Zayn the most wanted criminal on the entire Sword Coast.\nFortunately for Zayn, he had the attention of an important king, who made sure he was always protected from scrutiny. Zayn never learned he was being investigated, or even that his plan had had any adverse consequences at all, living out his life in a state of blissful richness, surrounded by family and happy to receive frequent visits from friends at his lavish estate." }
    , { name = "Knifery", pos = { x = 55, y = 510 }, text = "Knifery went on to live a life of boredom. There weren’t wars to fight. There weren’t people to kill. There weren’t even knives to be sharpened. After years of peace, Knifery finally began to accept that he needed to find something else to be good at.\nExcept then an old friend came to him with a special hit: a quest to kill a God. Could Gods even die? And if they could, could they be killed by knives?\nKnifery intended to find out.\nIt turned out they could, if you were good enough. It was the most exhilarating battle of Knifery’s life, and when it was over, he found he wanted another. And another. And another. The gods began to come together to defeat him after a point, but even then he was unstoppable.\nThe deities had to look for methods to escape the Immortal Realms where they resided. Many of them escaped, but a small army of them remained behind for one final, almighty battle with Knifery Shankefellow. Legends say that the fight lasted a millennium, and that by the end of it Knifery himself had ascended to godhood.\n He was never seen again, but it is said that if you look into the stars, sometimes you can see his face staring down at you from above." }
    , { name = "Aveline", pos = { x = 360, y = 554 }, text = "Aveline fell on hard times almost immediately, as thieves without parties tend to do. Luckily for her, she was used to having to steal to make ends meet, and this is precisely what she set herself to doing. She made an enemy of the local police force and had regular hijinx with them. It became a routine both sides enjoyed.\nTwo days before the police chief’s retirement, however, he died of a heart attack while chasing Aveline. By laws of succession, this made her the new police chief. At first she approached the role in bad faith, but quickly found she had a knack for solving murders thanks to a technique she had learned in her adventuring days: just hiring someone to speak to the ghost of the victim. Under her leadership, the crime rate in the city plummeted.\n People started to tell her she should run for Grand Paladin. She was hesitant to start a campaign for the position, but under the urging of the citizenry she did eventually run. Despite having no qualifications, she won the election 90-10 against the experienced, progressive and highly unpopular incumbent.\n Unfortunately she really didn’t know what she was doing as the head of a legendary paladin order and wound up leaving most things in the control of the corrupt paladins that filled her cabinet. Crime decreased, at least, as her ghost-talking strategy was placed into effect across the entire Forgotten Realms.\nThe major benefit to Aveline ended up being the boost in her renown. Suddenly she found she had the means and popularity to kick off the acting career she’d always dreamed of. Seeing there being no possible issue with a celebrity-elected-ruler, she set about making herself the most famous actress the Sword Coast has ever seen.\nHer movie “The Case of the Tavern Murder” made enough money for her to retire on the spot and still live in luxury for the rest of her life. She had seen what happened to people when they announced retirement, though, and so stayed happily making movies for the rest of her life. In particular, her surrealist film, “The Farmhand,” went on to redefine movies for a generation, thanks in part to a Fantasy-Oscar-winning soundtrack composed by an old friend." }
    , { name = "Yannick", pos = { x = 1276, y = 533 }, text = "Some time after the campfire concluded, Yannick went off in search of a new place to live, closer to the city. Luckily for him, he began his search right as new laws had been passed lowering rent costs all across the Sword Coast. Unfortunately, this had also led landlords to crack down on who they let rent. Powerful liches, especially those with pets, were not wanted.\nYannick was offended. Not about the lich thing, that he understood. But no pets allowed? Intolerable. As penance, Yannick did the only form of social protest he knew: he resurrected a whole shitload of animals and swarmed them over the landlords.\nThis method proved effective, and in fact there are very few problems that a horde of zombie animals cannot intimidate their way through, but it left Yannick with a new problem - getting rid of all these zombie animals.\nHe opened an adoption agency, which he ran out of the apartment he had just rented. Fortunately for him, interest in immortality had recently been piqued and he quickly ran out of pets.\nFrom there the next business move was simple - he opened a veterinary service for immortal animals, fixing up broken bones and ruptured brains from 9-5, 7 days a week (with days off for holidays).\nIt was a little strange to him that the animals always showed up without their owners, but he didn’t feel any pressure to pry. As the only practitioner for, well, anywhere, business was always booming and he lived out a fulfilling (after)life with his two friends - a dog and a cat." }
    , { name = "Os", pos = { x = 853, y = 399 }, text = "After the campfire, Os found that he had seen just about all he needed to see in the Forgotten Realms. His time there had been informative and perhaps even fun, but now it was time to return to his true dimension.\nOn his way there he met an old friend in the Dimension Between Dimensions and was told of a horrific calamity that was to befall the Realms he had just left - Bahamut was going to kill a god! Unfortunately for Os this was a catastrophic misunderstanding, but, unaware of this, Os went ahead and put the most reliable person he knew in charge of killing Bahamut in order to ensure peace in the Forgotten Realms.\nBack in his home dimension, Os found himself a bit of a celebrity. The Forgotten Realms were a largely uncharted place and everyone wanted to know what they were like. Os ended up publishing a book about his time there, titled “The Farmhand”, which became such a cultural zeitgeist that its influenced leaked into other dimensions, included the Forgotten Realms.\n His success gave him the funding and support to set off on a new adventure, to a strange planet called “Earth”, where some say he lives to this day, perfectly assimilated into rural culture." }
    , { name = "Mask", pos = { x = 1084, y = 285 }, text = "The Mask’s quiet sense of justice led her to pursue a career in law enforcement after the campfire. It took her a while to get through the police academy, but she graduated top of her class and was happily taken into the local police force. It wasn’t long before disaster struck the department, however.\nTwo days before he was set to retire, the chief of police died pursuing a criminal, and The Mask had to watch, aghast, as the very criminal he had died trying to apprehend took over as the new police chief.\nWorse, The Mask was so good at her job that it made the police chief look good, too. The Mask was an expert at stealth, surveillance, and subterfuge. Her pacifist approach to crime fighting single-handedly brought down police brutality in the precinct. She had a quiet, solemn way of interrogating people which, along with her weird masks, always managed to make the perp break down and confess.\nSure, the new police chief came up with a novel new way to solve crimes, but The Mask felt she brought the real innovation.\nLuckily for her, a strange movement formed to oust the police chief into a more glamorous career path, leaving a vacancy that only The Mask could fill. Under her careful direction, the local area became the most peaceful and quiet on the entire Sword Coast, with a crime rate of 0%. The Mask lived out her life there quietly and contentedly." }
    , { name = "Samara", pos = { x = 586, y = 463 }, text = "After the campfire, Samara was struck by the harsh reality that she had no employable skills outside of playing annoying melodies. She struggled to find a way to make money until eventually she heard news that a major corporation was opening its doors to evil and chaotic employees. Anyone was welcome!\nShe quickly got a job there and then set herself to doing her only skill: playing annoying melodies while everyone else around her worked hard. It was through her penchant for wandering aimlessly around the company and frustrating people that she was discovered by an evil talent agent looking for someone to create tunes so catchy that little kids would get them stuck in their head and want to buy the products the songs advertised.\nSamara was incredible at this, and pretty soon was being scouted all over the realm. Everyone wanted her to make music for their ads, but her sights were on the big screen.\nSamara had the strange inspiration to make a song about a farmhand, which quickly caught on due to a rising interest in farmhands in the Forgotten Realms. A famous actress even used it in a popular surrealist film, then took Samara on to make music for the rest of her movies.\nSamara did that for a while, but retired in luxury a few years later after releasing her hit album, Wunderbarrier III." }
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
            model.stepCount - dialogueData.dialogueStart


stepsPerChar =
    2


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


buttonEvent : Model -> PartyMember -> List (Html.Attribute Msg)
buttonEvent model char =
    if model.dialogueState /= NoDialogue || model.fadeOutStart /= Nothing then
        []

    else
        [ Html.Events.onMouseDown (SetDialogue char) ]


fadeDuration =
    100


buttonOpacity : Model -> String -> Html.Attribute msg
buttonOpacity model buttonName =
    case model.fadeOutStart of
        Just ( character, startStep ) ->
            if buttonName == character then
                String.fromFloat (1 - toFloat (model.stepCount - startStep) / fadeDuration) |> Html.Attributes.style "opacity"

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
                    Html.img
                        (buttonEvent model a
                            ++ [ Html.Attributes.class a.name
                               , Html.Attributes.src ("/images/" ++ a.name ++ ".png")
                               , style "left" (px a.pos.x)
                               , style "top" (px a.pos.y)
                               , style "position" "fixed"
                               , buttonOpacity model a.name
                               ]
                        )
                        [ text a.name ]
                )
                visibleCharacters

        nextButtonHtml =
            let
                notNextLine =
                    (toFloat (timeElapsed model) / toFloat stepsPerChar |> floor) <= String.length (getCurrentLine model)
            in
            if notNextLine then
                []

            else
                [ Html.img
                    [ Html.Events.onMouseDown DialogueNext
                    , Html.Attributes.src "/images/buttonNext.png"
                    , style "position" "absolute"
                    , style "top" "256px"
                    , style "left" "1550px"
                    ]
                    [ text "nextButton" ]
                ]

        dialogueBoxHtml =
            if model.dialogueState == NoDialogue then
                []

            else
                [ div
                    [ style "position" "fixed"
                    , style "left" (px 68)
                    , style "top" (px 699)
                    ]
                    ([ Html.img [ Html.Attributes.src "/images/dialoguebox.png" ] [] ]
                        ++ [ div
                                [ style "position" "absolute"
                                , style "top" "50%"
                                , style "left" "90px"
                                , style "width" "1600px"
                                , style "font-size" "250%"
                                , style "font-family" "georgia"
                                , style "transform" "translate(0%, -50%)"
                                ]
                                [ textToHtml (getCurrentLine model) (timeElapsed model) ]
                           ]
                        ++ nextButtonHtml
                    )
                ]
    in
    div [ style "position" "fixed"
        , style "left" (px 0)
        , style "top" (px 0)
        ]
        ([ Html.img [ Html.Attributes.src "/images/0campfire.png" ] [] ]
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
