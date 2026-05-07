module Main exposing (Actor(..), Model, Msg(..), MyCurve(..), Point, State(..), add, addCurve, addPull, addTurtle, angle, approachesOne, backgroundElements, bubbleClr, buttonGreen, buttonHighlight, cody, codyS, completeMermaid, completeSadMermaid, debugSpeedup, drawCurve, eyeShellS, failedEndScript, finalMermaid, finalSadMermaid, fish, fishAni, fishBackground, fishS, fishTail, fishTailS, flippers, flippersTurtle, headline, hindLimb, init, interpolateFloats, jellyfish, leftFin, leftFinS, liftOff, liftOff2, main, mermaid, mkAnimationPiece, mkAnimationPieceVS, mkScriptFor, mouthShellS, movingStarfish, multiBubbleLeft, multiBubbleRight, myBlack, myRepeat, myShapes, parrotFish, parrotFishS, partyHat, pearls, pufferfish, pufferfishS, q0, q1, q2, q3, rainbow, restartButton, rightFin, rightFinS, sadMermaid, sandGradient, scaleCurve, scaleP, scalePull, scaleTurtle, scriptSpeed, seaweedRock, shell, spike, spikeS, starfish, stops, stopsStarfish, sub, subCurve, subPull, subTurtle, tail, tailPuffer, tailS, tailSad, tentacles, textBlue, textHeight, textSpeed, treasureChess, turtle, update, upperFin, upperFinS, view, water, welcomeScript, xCoord, yCoord, yellowCoral, yellowCoralTotal)

import Array
import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)
import Html
import Html.Attributes as Html


debugSpeedup =
    1


type Actor
    = Mermaid
    | Pixar
    | Cody
    | Cloonie
    | Nar
    | Either
    | Or



-- VERSION 1 PROTOTYPE
{- WHAT THIS VERSION FEATURES:
   - Scripts on every scene (shells scripts too)
   - Replay button on every scene
   - Cloonie on every scene
   - Transitions to all scenes
   - Victory scene
   -
-}


welcomeScript =
    [ ( Mermaid, "Let's Practice Social-Emotional" )
    , ( Mermaid, "Learning! Are You Ready?" )
    ]


failedEndScript =
    [ ( Mermaid, " " )
    , ( Mermaid, " " )
    , ( Mermaid, "Aww... The three best friends" )
    , ( Mermaid, "never spoke to each other ever" )
    , ( Mermaid, "again... :(" )
    , ( Mermaid, " " )
    , ( Mermaid, "Do you want to try again to" )
    , ( Mermaid, "save the friendship?" )
    , ( Mermaid, " " )
    ]


myShapes model =
    case model.state of
        Welcome showInstructions ->
            [ backgroundElements 1 (\input -> group []) model

            {--- coral
            , yellowCoral blue 6
            |> scale 0.8 
            |> move (-40, -50)-}
            -- starfish
            , movingStarfish model
                |> scale 0.25
                |> scale (0.7 + 0.2 * sin (1.5 * model.time))
                |> rotate (5 * sin (1 * model.time))
                |> move ( -60, -50 )

            -- jellyfish
            , jellyfish (hsl (degrees 30 * model.time) 1 0.5)
                |> scale 0.5
                |> move ( 30, -10 )
                |> move ( 0, 30 * cos model.time * 0.3 )
                |> move ( repeatDistance 5 -500 100 -model.time, 20 )

            -- text
            , [ multiBubbleRight black welcomeScript 42 Mermaid 4.5
                    --(model.time - model.startTime)
                    |> move ( -10, 16 )
                    |> scale 1.7

              -- mermaid
              , finalMermaid model
                    |> scale 0.8
                    |> move ( 120, -17 )
              ]
                |> group
                |> move ( 0, 10 * sin model.time * 0.5 )

            -- let's go
            , shell "Let's Go!" WelcomeTo1
                |> scale 0.5
                |> move ( 0, -40 )
            ]
                ++ (if showInstructions then
                        [ [ rect 190 125 |> filled (rgba 255 255 255 0.85)
                          , headline "Help the mermaid"
                                |> move ( 0, 35 )
                          , headline "say the right thing"
                                |> move ( 0, 15 )
                          , headline "by clicking"
                                |> move ( 0, -5 )
                          , shell "the" HideInstructions
                                |> scale 0.35
                                |> move ( -40, -30 )
                          , shell "right" HideInstructions
                                |> scale 0.35
                                |> move ( 0, -30 )
                          , shell "choice" HideInstructions
                                |> scale 0.35
                                |> move ( 40, -30 )
                          ]
                            |> group
                            |> notifyTap HideInstructions
                        ]

                    else
                        []
                   )

        Scene1 ->
            let
                scriptTime =
                    debugSpeedup * (model.time - model.startTime)

                scene1Script =
                    pixarScript
                        ++ codyScript
                        ++ mermaidScript
                        ++ eitherScript
                        ++ orScript

                pixarScript =
                    [ ( Pixar, "Hey guys! It's been 5 years" )
                    , ( Pixar, "since we met in Kindergarten!" )
                    , ( Pixar, " " )
                    , ( Pixar, "I think for today, we should" )
                    , ( Pixar, "do something special..." )
                    ]

                codyScript =
                    [ ( Cody, "OH! I know!" )
                    , ( Cody, " " )
                    , ( Cody, "Why don't we build" )
                    , ( Cody, "a statue out of pearls" )
                    , ( Cody, "to celebrate?" )
                    ]

                mermaidScript =
                    [ ( Mermaid, "Hmm, what do you think, Cloonie?" ) ]

                eitherScript =
                    [ ( Either, "Ummm..." )
                    , ( Either, "but how in the" )
                    , ( Either, "sea are we " )
                    , ( Either, "supposed to " )
                    , ( Either, "build something " )
                    , ( Either, "like that??? " )
                    , ( Either, "We're only " )
                    , ( Either, "in Grade 4, " )
                    , ( Either, "we can't really" )
                    , ( Either, "do anything. " )
                    , ( Either, "Let's just go " )
                    , ( Either, "eat seaweed, " )
                    , ( Either, "it's easier..." )
                    , ( Either, "" )
                    , ( Either, "" )
                    ]

                orScript =
                    [ ( Or, "Oh my gosh," )
                    , ( Or, "5 YEARS?!" )
                    , ( Or, "Yay!" )
                    , ( Or, "That sounds" )
                    , ( Or, "so exciting!" )
                    , ( Or, "We're going to" )
                    , ( Or, "have the best " )
                    , ( Or, "time ever!!" )
                    , ( Or, "" )
                    ]
            in
            [ backgroundElements 1 fishAni model

            --pixar
            , parrotFish model
                |> scale 0.5
                |> move ( -68, -3 )
            , multiBubbleLeft black scene1Script 46 Pixar scriptTime
                |> move ( -8, 25 )
            , cody model orange
                |> move ( -10, -8 )
            , multiBubbleRight black scene1Script 42 Cody scriptTime
                |> move ( -50, 25 )
            , [ multiBubbleRight black scene1Script 48 Mermaid scriptTime
                    |> move ( 0, 35 )
                    |> scale 1
              , finalMermaid model
                    |> scale 0.8
                    |> move ( 120, -17 )
              ]
                |> group
                |> move ( 0, 10 * sin model.time * 0.5 )
            , pufferfish
                |> mirrorX
                |> scale 0.75
                |> move ( 78, 48 )
            , shell "Either say:" T1to2
                |> move ( -130, -80 )
                |> scale 0.5
            , group
                [ multiBubbleLeft textBlue
                    scene1Script
                    21
                    Either
                    scriptTime
                    |> move ( 47, 0 )
                ]
                |> move ( -68, -40 )
            , shell "or say:" T1to3
                |> move ( 60, -80 )
                |> scale 0.5
            , group
                [ multiBubbleLeft textBlue
                    scene1Script
                    20
                    Or
                    scriptTime
                    |> move ( 47, 0 )
                ]
                |> move ( 30, -40 )
            , if scriptTime > 85 then
                restartButton model.time
                    |> move ( 80, 55 )
                    |> notifyTap Restart1

              else
                group []
            ]

        Scene2 ->
            let
                scriptTime =
                    debugSpeedup * (model.time - model.startTime)

                scene2Script =
                    pixar2Script ++ cody2Script ++ pixar2moreScript ++ mermaid2Script ++ either2Script ++ or2Script

                pixar2Script =
                    [ ( Pixar, "Then why don't we go and" )
                    , ( Pixar, "collect the shells next to" )
                    , ( Pixar, "Seaweed Seashore and make" )
                    , ( Pixar, "a statute out of seashells instead?" )
                    , ( Pixar, "Then we can go and eat" )
                    , ( Pixar, "AND celebrate?" )
                    ]

                cody2Script =
                    [ ( Cody, "Umm.... I'm not sure..." )
                    , ( Cody, "Cloonie makes a good point..." )
                    ]

                pixar2moreScript =
                    [ ( Pixar, "Come on! It'll be something" )
                    , ( Pixar, "exciting for sure" )
                    ]

                mermaid2Script =
                    [ ( Mermaid, " Cloonie, what response" )
                    , ( Mermaid, "do you think will best" )
                    , ( Mermaid, "support Pixar and Cody?" )
                    ]

                either2Script =
                    [ ( Either, "Nah," )
                    , ( Either, "that sounds" )
                    , ( Either, "too difficult," )
                    , ( Either, "I think we" )
                    , ( Either, "should just" )
                    , ( Either, "give up on" )
                    , ( Either, "the statue" )
                    , ( Either, "idea." )
                    , ( Either, "" )
                    , ( Either, "" )
                    ]

                or2Script =
                    [ ( Or, "Well...I guess." )
                    , ( Or, "But I'd" )
                    , ( Or, "definitely" )
                    , ( Or, "like to eat" )
                    , ( Or, "some" )
                    , ( Or, "seaweed before" )
                    , ( Or, "we start working." )
                    , ( Or, "I really need" )
                    , ( Or, "some energy in" )
                    , ( Or, "my body." )
                    , ( Or, "" )
                    , ( Or, "" )
                    ]
            in
            [ backgroundElements 1 fishAni model
            , pufferfish
                |> mirrorX
                |> scale 0.75
                |> move ( 78, 48 )

            --pixar
            , parrotFish model
                |> scale 0.5
                |> move ( -68, -3 )
            , multiBubbleLeft black scene2Script 46 Pixar scriptTime
                |> move ( -8, 25 )
            , cody model orange
                |> move ( -10, -8 )
            , multiBubbleRight black scene2Script 42 Cody scriptTime
                |> move ( -50, 25 )
            , [ multiBubbleRight black scene2Script 48 Mermaid scriptTime
                    |> move ( 0, 35 )
                    |> scale 1
              , finalMermaid model
                    |> scale 0.8
                    |> move ( 120, -17 )
              ]
                |> group
                |> move ( 0, 10 * sin model.time * 0.5 )
            , shell "Either say:" T2toend
                |> move ( -130, -80 )
                |> scale 0.5
            , group
                [ multiBubbleLeft textBlue
                    scene2Script
                    21
                    Either
                    scriptTime
                    |> move ( 47, 0 )
                ]
                |> move ( -68, -40 )
            , shell "or say:" T2to3
                |> move ( 60, -80 )
                |> scale 0.5
            , group
                [ multiBubbleLeft textBlue
                    scene2Script
                    20
                    Or
                    scriptTime
                    |> move ( 47, 0 )
                ]
                |> move ( 30, -40 )
            , if scriptTime > 20 then
                restartButton model.time
                    |> move ( 80, 55 )
                    |> notifyTap Restart2

              else
                group []
            ]

        Scene3 ->
            let
                scriptTime =
                    debugSpeedup * (model.time - model.startTime)

                scene3Script =
                    mermaid3Script ++ pixar3Script ++ cody3Script ++ mermaid3twoScript ++ cody3twoScript ++ pixar3twoScript ++ mermaid3threeScript ++ either3Script ++ or3Script

                mermaid3Script =
                    [ ( Mermaid, "*15 Minutes Later*" ) ]

                pixar3Script =
                    [ ( Pixar, "Let's split up and collect" )
                    , ( Pixar, "as many shells as we can!" )
                    ]

                cody3Script =
                    [ ( Cody, "Yeah!" ) ]

                mermaid3twoScript =
                    [ ( Mermaid, "*1 Hour Later*" ) ]

                cody3twoScript =
                    [ ( Cody, "Alright, let's check in everyone!" )
                    , ( Cody, "these are all the seashells" )
                    , ( Cody, "that we got! Why don't" )
                    , ( Cody, "we try building three friends" )
                    , ( Cody, "hugging each other?" )
                    ]

                pixar3twoScript =
                    [ ( Pixar, "Yeah! Why don't we each make" )
                    , ( Pixar, "check in often and at the" )
                    , ( Pixar, "end, we should work together" )
                    , ( Pixar, "to combine the three of them." )
                    ]

                mermaid3threeScript =
                    [ ( Mermaid, "What do you think" )
                    , ( Mermaid, "about that,\u{00A0}Cloonie?" )
                    ]

                either3Script =
                    [ ( Either, "Ugh," )
                    , ( Either, "you guys are" )
                    , ( Either, "so lame." )
                    , ( Either, "Why do we" )
                    , ( Either, "even need" )
                    , ( Either, "to check in?" )
                    , ( Either, "We can just make" )
                    , ( Either, "each statue" )
                    , ( Either, "ourself" )
                    , ( Either, "and figure" )
                    , ( Either, "things out" )
                    , ( Either, "on our own." )
                    , ( Either, "We'll combine" )
                    , ( Either, "them later." )
                    , ( Either, "" )
                    , ( Either, "" )
                    ]

                or3Script =
                    [ ( Or, "Mhm," )
                    , ( Or, "that sounds" )
                    , ( Or, "great!" )
                    , ( Or, "Checking in" )
                    , ( Or, "with each" )
                    , ( Or, "other sounds" )
                    , ( Or, "like an" )
                    , ( Or, "excellent" )
                    , ( Or, "idea, Pixar!" )
                    , ( Or, "Or else," )
                    , ( Or, "we'll end" )
                    , ( Or, "up making" )
                    , ( Or, "things that" )
                    , ( Or, "don't fit" )
                    , ( Or, "in with" )
                    , ( Or, "each other!" )
                    , ( Or, "" )
                    ]
            in
            [ backgroundElements 0.95 (\input -> circle 5 |> filled orange) model

            --pixar
            , parrotFish model
                |> scale 0.5
                |> move ( -68, -3 )
            , pufferfish
                |> mirrorX
                |> scale 0.75
                |> move ( 78, 48 )
            , multiBubbleLeft black scene3Script 46 Pixar scriptTime
                |> move ( -8, 25 )
            , cody model orange
                |> move ( -10, -8 )
            , multiBubbleRight black scene3Script 42 Cody scriptTime
                |> move ( -50, 25 )
            , [ multiBubbleRight black scene3Script 48 Mermaid scriptTime
                    |> move ( 0, 35 )
                    |> scale 1
              , finalMermaid model
                    |> scale 0.8
                    |> move ( 120, -17 )
              ]
                |> group
                |> move ( 0, 10 * sin model.time * 0.5 )
            , shell "Either say:" T3to4
                |> move ( -130, -80 )
                |> scale 0.5
            , group
                [ multiBubbleLeft textBlue
                    scene3Script
                    20
                    Either
                    scriptTime
                    |> move ( 47, 0 )
                ]
                |> move ( -68, -40 )
            , shell "or say:" T3to5
                |> move ( 60, -80 )
                |> scale 0.5
            , group
                [ multiBubbleLeft textBlue
                    scene3Script
                    20
                    Or
                    scriptTime
                    |> move ( 47, 0 )
                ]
                |> move ( 30, -40 )
            , if scriptTime > 85 then
                restartButton model.time
                    |> move ( 80, 55 )
                    |> notifyTap Restart3

              else
                group []
            ]

        Scene2End ->
            let
                scriptTime =
                    debugSpeedup * (model.time - model.startTime)
            in
            [ backgroundElements 0.9 (\input -> group []) model

            {--- coral
            , yellowCoral blue 6
            |> scale 0.8 
            |> move (-40, -50)-}
            , pufferfish
                |> mirrorX
                |> scale 0.75
                |> move ( 78, 48 )

            -- starfish
            , movingStarfish model
                |> scale 0.25
                |> scale (0.7 + 0.2 * sin (1.5 * model.time))
                |> rotate (5 * sin (1 * model.time))
                |> move ( -60, -50 )

            -- text
            , [ multiBubbleRight black failedEndScript 42 Mermaid (repeatDistance 1 20 1 model.time)
                    --(model.time - model.startTime)
                    |> move ( -10, 16 )
                    |> scale 1.7

              -- sad mermaid
              , finalSadMermaid model
                    |> scale 0.8
                    |> move ( 120, -17 )
              ]
                |> group
                |> move ( 0, 7 * sin model.time * 0.5 )

            -- shell movement return back to beginning
            , shell "Yes Please!" End2ToWelcome
                |> scale 0.5
                |> move ( 0, -40 )
            , if scriptTime > 85 then
                restartButton model.time
                    |> move ( 80, 55 )
                    |> notifyTap Restart2end

              else
                group []
            ]

        Scene4 ->
            let
                scriptTime =
                    debugSpeedup * (model.time - model.startTime)

                scene4Script =
                    pixar4Script
                        ++ mermaid4Script
                        ++ pixar4twoScript
                        ++ mermaid4twoScript
                        ++ pixar4threeScript
                        ++ cody4Script
                        ++ pixar4fourScript
                        ++ mermaid4threeScript
                        ++ either4Script
                        ++ or4Script

                pixar4Script =
                    [ ( Pixar, "Hmm, I guess that works too." ) ]

                mermaid4Script =
                    [ ( Nar, "30 minutes later" ) ]

                pixar4twoScript =
                    [ ( Pixar, "Good job guys! I think we" )
                    , ( Pixar, "can put them together now." )
                    ]

                mermaid4twoScript =
                    [ ( Nar, "*After they're put together*" ) ]

                pixar4threeScript =
                    [ ( Pixar, "Oh no! These figures" )
                    , ( Pixar, "are all different sizes\u{00A0}and they" )
                    , ( Pixar, "don't look like they're hugging." )
                    ]

                cody4Script =
                    [ ( Cody, "we try building three friends" )
                    , ( Cody, "hugging each other?" )
                    ]

                pixar4fourScript =
                    [ ( Pixar, "Yeah! Why don't we each make" )
                    , ( Pixar, "check in often and at the" )
                    , ( Pixar, "end, we should work together" )
                    , ( Pixar, "to combine the three of them." )
                    ]

                mermaid4threeScript =
                    [ ( Mermaid, "What do you think" )
                    , ( Mermaid, "about that,\u{00A0}Cloonie?" )
                    ]

                either4Script =
                    [ ( Either, "Just put" )
                    , ( Either, "them besides" )
                    , ( Either, "each other." )
                    , ( Either, "They don't" )
                    , ( Either, "need to be" )
                    , ( Either, "hugging each" )
                    , ( Either, "other, anyways." )
                    , ( Either, "*All three put" )
                    , ( Either, "the statues" )
                    , ( Either, "beside each" )
                    , ( Either, "other*" )
                    , ( Either, "Ugh," )
                    , ( Either, "this was" )
                    , ( Either, "the most" )
                    , ( Either, "boring day" )
                    , ( Either, "of my life!" )
                    , ( Either, "" )
                    , ( Either, "" )
                    ]

                or4Script =
                    [ ( Or, "Oh my gosh," )
                    , ( Or, "5 YEARS?!" )
                    , ( Or, "Yay!" )
                    , ( Or, "That sounds" )
                    , ( Or, "so exciting!" )
                    , ( Or, "We're going to" )
                    , ( Or, "have the best " )
                    , ( Or, "time ever!!" )
                    , ( Or, "" )
                    ]
            in
            [ backgroundElements 0.8 (\input -> circle 5 |> filled purple) model
            , pufferfish
                |> mirrorX
                |> scale 0.75
                |> move ( 78, 48 )

            --pixar
            , parrotFish model
                |> scale 0.5
                |> move ( -68, -3 )
            , multiBubbleLeft black scene4Script 46 Pixar scriptTime
                |> move ( -8, 25 )
            , cody model orange
                |> move ( -10, -8 )
            , multiBubbleRight black scene4Script 42 Cody scriptTime
                |> move ( -50, 25 )
            , [ multiBubbleRight black scene4Script 48 Mermaid scriptTime
                    |> move ( 0, 35 )
                    |> scale 1
              , finalMermaid model
                    |> scale 0.8
                    |> move ( 120, -17 )
              ]
                |> group
                |> move ( 0, 10 * sin model.time * 0.5 )
            , shell "Either say:" T4toend
                |> move ( -130, -80 )
                |> scale 0.5

            -- TODO  add characters, and repeat button
            , group
                [ multiBubbleLeft textBlue
                    scene4Script
                    20
                    Either
                    scriptTime
                    |> move ( 47, 0 )
                ]
                |> move ( -68, -40 )
            , shell "or say:" T4to4half
                |> move ( 60, -80 )
                |> scale 0.5
            , group
                [ multiBubbleLeft textBlue
                    scene4Script
                    20
                    Or
                    scriptTime
                    |> move ( 47, 0 )
                ]
                |> move ( 30, -40 )
            , if scriptTime > 85 then
                restartButton model.time
                    |> move ( 80, 55 )
                    |> notifyTap Restart4

              else
                group []
            ]

        Scene5 ->
            let
                scriptTime =
                    debugSpeedup * (model.time - model.startTime)

                scene5Script =
                    mermaid5Script
                        ++ pixar5Script
                        ++ mermaid5twoScript
                        ++ pixar5twoScript
                        ++ cody5Script
                        ++ either5Script

                mermaid5Script =
                    [ ( Mermaid, "And so, each of the three friends" )
                    , ( Mermaid, "starts building a statue of" )
                    , ( Mermaid, "themselves out of seashells," )
                    , ( Mermaid, "making sure to check-in" )
                    , ( Mermaid, "with each other often." )
                    , ( Mermaid, "1 hour later..." )
                    ]

                pixar5Script =
                    [ ( Pixar, "Good job guys!" )
                    , ( Pixar, "I think we can put" )
                    , ( Pixar, "them together now!" )
                    ]

                mermaid5twoScript =
                    [ ( Mermaid, "The three friends starts putting" )
                    , ( Mermaid, "them together" )
                    ]

                pixar5twoScript =
                    [ ( Pixar, "Whoaaaaa! Look!" ) ]

                cody5Script =
                    [ ( Cody, "That looks amazing!" )
                    , ( Cody, "Let's place it at the" )
                    , ( Cody, "center of our three houses!" )
                    , ( Cody, "It was such a good idea" )
                    , ( Cody, "to make it out of sand instead" )
                    , ( Cody, "too!!!" )
                    ]

                either5Script =
                    [ ( Either, "Woohoo!!" )
                    , ( Either, "This looks" )
                    , ( Either, "so good!" )
                    , ( Either, "I'm so happy" )
                    , ( Either, "that we" )
                    , ( Either, "were able" )
                    , ( Either, "to work" )
                    , ( Either, "together," )
                    , ( Either, "this was" )
                    , ( Either, "the best" )
                    , ( Either, "day of" )
                    , ( Either, "my life!" )
                    , ( Either, "" )
                    , ( Either, "" )
                    ]
            in
            [ backgroundElements 1.05 (\input -> circle 5 |> filled black) model

            -- sculptures (good one)
            , pufferfishS
                |> move ( 0, 10 )
                |> scale 0.5
                |> move ( 30, -30 )
            , parrotFishS { time = 0 }
                |> scale 0.6
                |> move ( -20, -20 )
                |> scale 0.5
                |> move ( 30, -30 )
            , codyS { time = 0 } purple
                |> mirrorX
                |> move ( 80, -35 )
                |> scale 0.5
                |> move ( 30, -30 )

            --pixar
            , pufferfish
                |> mirrorX
                |> scale 0.75
                |> move ( 78, 48 )
            , parrotFish model
                |> scale 0.5
                |> move ( -68, -3 )
            , multiBubbleLeft black scene5Script 46 Pixar scriptTime
                |> move ( -8, 25 )
            , cody model orange
                |> move ( -10, -8 )
            , multiBubbleRight black scene5Script 42 Cody scriptTime
                |> move ( -50, 25 )
            , [ multiBubbleRight black scene5Script 48 Mermaid scriptTime
                    |> move ( 0, 35 )
                    |> scale 1
              , finalMermaid model
                    |> scale 0.8
                    |> move ( 120, -17 )
              ]
                |> group
                |> move ( 0, 10 * sin model.time * 0.5 )
            , shell "Let's Party!" T5toFINALend
                |> move ( -30, -80 )
                |> scale 0.5
            , group
                [ multiBubbleLeft textBlue
                    scene5Script
                    20
                    Either
                    scriptTime
                    |> move ( 47, 0 )
                ]
                |> move ( -20, -40 )
            , if scriptTime > 85 then
                restartButton model.time
                    |> move ( 80, 55 )
                    |> notifyTap Restart5

              else
                group []
            ]

        Scene4End ->
            let
                scriptTime =
                    debugSpeedup * (model.time - model.startTime)

                scene4EndScript =
                    [ failedEndScript ]
            in
            [ backgroundElements 1.05 (\input -> circle 5 |> filled brown) model

            {--- coral
            , yellowCoral blue 6
            |> scale 0.8 
            |> move (-40, -50)-}
            , pufferfish
                |> mirrorX
                |> scale 0.75
                |> move ( 78, 48 )

            -- starfish
            , movingStarfish model
                |> scale 0.25
                |> scale (0.7 + 0.2 * sin (1.5 * model.time))
                |> rotate (5 * sin (1 * model.time))
                |> move ( -60, -50 )

            -- text
            , [ multiBubbleRight black failedEndScript 42 Mermaid (repeatDistance 1 20 1 model.time)
                    --(model.time - model.startTime)
                    |> move ( -10, 16 )
                    |> scale 1.7

              -- sad mermaid
              , finalSadMermaid { time = model.time * 0.25 }
                    |> scale 0.8
                    |> move ( 120, -17 )
              ]
                |> group
                |> move ( 0, 4 * sin model.time * 0.5 )

            -- shell movement return back to beginning
            , shell "Yes Please!" End4ToWelcome
                |> scale 0.5
                |> move ( 0, -40 )
            , if scriptTime > 85 then
                restartButton model.time
                    |> move ( 80, 55 )
                    |> notifyTap Restart4end

              else
                group []
            ]

        Scene4half ->
            let
                scriptTime =
                    debugSpeedup * (model.time - model.startTime)

                scene4halfScript =
                    pixar4halfScript

                pixar4halfScript =
                    [ ( Pixar, "That's fine, Cloonie." )
                    , ( Pixar, "Thank you for your apology." )
                    , ( Pixar, "Alright there's still time" )
                    , ( Pixar, "before night, let's do this!!" )
                    ]
            in
            [ backgroundElements 1 (\input -> circle 5 |> filled green) model

            -- TODO  add characters, and repeat button
            , pufferfish
                |> mirrorX
                |> scale 0.75
                |> move ( 78, 48 )

            --pixar
            , parrotFish model
                |> scale 0.5
                |> move ( -68, -3 )
            , multiBubbleLeft black scene4halfScript 46 Pixar scriptTime
                |> move ( -8, 25 )
            , cody model orange
                |> move ( -10, -8 )
            , [ multiBubbleRight black scene4halfScript 48 Mermaid scriptTime
                    |> move ( 0, 35 )
                    |> scale 1
              , finalMermaid model
                    |> scale 0.8
                    |> move ( 120, -17 )
              ]
                |> group
                |> move ( 0, 10 * sin model.time * 0.5 )
            , shell "Let's Go!" T4halfto5
                |> scale 0.5
                |> move ( 0, -40 )
            , if scriptTime > 85 then
                restartButton model.time
                    |> move ( 80, 55 )
                    |> notifyTap Restart4half

              else
                group []
            ]

        SceneEnd ->
            let
                scriptTime =
                    debugSpeedup * (model.time - model.startTime)

                sceneEndScript =
                    [ ( Mermaid, "Whew, thanks Cloonie!" )
                    , ( Mermaid, "You saved the friendship!" )
                    , ( Mermaid, "Congratulations! You won the game!" )
                    , ( Mermaid, "...AND learned social-emotional learning?!?!" )
                    , ( Mermaid, "Wow... you're so impressive!!" )
                    ]
            in
            [ backgroundElements 1 (\input -> circle 5 |> filled darkGreen) model

            {--- coral
            , yellowCoral blue 6
            |> scale 0.8 
            |> move (-40, -50)-}
            , pufferfish
                |> mirrorX
                |> scale 0.75
                |> move ( 78, 48 )

            -- pufferfish party hat
            , partyHat pink lightRed yellow green lightBlue blue
                |> scale 0.1
                |> rotate (degrees 30)
                |> move ( 73, 58 )

            --pixar
            , parrotFish model
                |> scale 0.5
                |> move ( -68, -3 )

            -- pixar party hat
            , partyHat pink lightRed yellow green lightBlue blue
                |> rotate (degrees -10)
                |> scale 0.2
                |> move ( -50, 10 )
            , cody model orange
                |> move ( -10, -8 )

            -- cody party hat
            , partyHat pink lightRed yellow green lightBlue blue
                |> rotate (degrees 10)
                |> scale 0.2
                |> move ( 0, 14 )
            , [ multiBubbleRight black sceneEndScript 48 Mermaid scriptTime
                    |> move ( 0, 35 )
                    |> scale 1
              , finalMermaid model
                    |> scale 0.8
                    |> move ( 120, -17 )
              , partyHat pink lightRed yellow green lightBlue blue
                    |> scale 0.2
                    |> move ( 75, 25 )
              ]
                |> group
                |> move ( 0, 10 * sin model.time * 0.5 )

            -- starfish
            , movingStarfish model
                |> scale 0.25
                |> scale (0.7 + 0.2 * sin (1.5 * model.time))
                |> rotate (5 * sin (1 * model.time))
                |> move ( -60, -50 )

            -- jellyfish
            , jellyfish (hsl (degrees 30 * model.time) 1 0.5)
                |> scale 0.5
                |> move ( 30, -10 )
                |> move ( 0, 30 * cos model.time * 0.3 )
                |> move ( repeatDistance 5 -500 100 -model.time, 20 )
            , if scriptTime > 85 then
                restartButton model.time
                    |> move ( 80, 55 )
                    |> notifyTap Restartend

              else
                group []
            ]


headline txt =
    text txt
        |> size 10
        |> centered
        |> sansserif
        |> filled (rgb 0 100 150)
        |> addOutline (solid 0.5) (rgb 50 155 255)


textSpeed =
    1.85


bubbleClr =
    rgba 255 255 255 0.85


textHeight =
    4


multiBubbleLeft : Color -> List ( Actor, String.String ) -> Float -> Actor -> Float -> Shape userMsg
multiBubbleLeft textClr script width actor t =
    let
        bubble =
            curve ( -width * 0.5, -8 )
                [ Pull ( -width * 0.5 + 5, -8 ) ( -width * 0.5 + 7, -4 )
                , Pull ( width * 0.5 + 2, -8 ) ( width * 0.5, 1 )
                , Pull ( width * 0.5, 8 ) ( 0, 7 )
                , Pull ( -width * 0.5, 8 ) ( -width * 0.5, 2 )
                , Pull ( -width * 0.5, -2 ) ( -width * 0.5 + 3, -3 )
                , Pull ( -width * 0.5 + 5, -6 ) ( -width * 0.5 + 0, -8 )
                ]

        ( contents, visibility ) =
            mkScriptFor textClr actor script (width / 15)

        isVisible =
            List.any (\start -> textSpeed * 0.75 * t > scriptSpeed * 0.75 * start - 2 && textSpeed * t < scriptSpeed * start + 4) visibility
    in
    scale 2 <|
        group <|
            if isVisible then
                [ bubble |> filled bubbleClr
                , contents
                    |> move ( 1.5 - 0.5 * width, textSpeed * t )
                    |> clip (bubble |> ghost)
                , bubble |> outlined (solid 0.5) textClr
                ]

            else
                []


multiBubbleRight : Color -> List ( Actor, String.String ) -> Float -> Actor -> Float -> Shape userMsg
multiBubbleRight textClr script width actor t =
    let
        bubble =
            curve ( -width * 0.5, -8 )
                [ Pull ( -width * 0.5 + 5, -8 ) ( -width * 0.5 + 7, -4 )
                , Pull ( width * 0.5 + 2, -8 ) ( width * 0.5, 1 )
                , Pull ( width * 0.5, 8 ) ( 0, 7 )
                , Pull ( -width * 0.5, 8 ) ( -width * 0.5, 2 )
                , Pull ( -width * 0.5, -2 ) ( -width * 0.5 + 3, -3 )
                , Pull ( -width * 0.5 + 5, -6 ) ( -width * 0.5 + 0, -8 )
                ]

        --bubble = curve (-40,-8) [Pull (-35,-8) (-33,-4), Pull (42,-8) (40,1), Pull (40,8) (0, 7), Pull (-40,8) (-40,2), Pull (-40,-2) (-37,-3), Pull (-35,-6) (-40,-8) ]
        ( contents, visibility ) =
            mkScriptFor textClr actor script (width / 15)

        isVisible =
            List.any (\start -> textSpeed * 0.75 * t > scriptSpeed * 0.75 * start - 2 && textSpeed * t < scriptSpeed * start + 4) visibility
    in
    scale 2 <|
        group <|
            if isVisible then
                [ bubble |> filled bubbleClr |> mirrorX
                , contents
                    |> move ( 1 - 0.5 * width, textSpeed * t )
                    |> clip (bubble |> ghost |> mirrorX)
                , bubble |> outlined (solid 0.25) textClr |> mirrorX
                ]

            else
                []


mkScriptFor textClr actor theScript offset =
    let
        mkTxt idx txt =
            text txt |> size 2 |> fixedwidth |> filled textClr |> move ( offset, -textHeight * idx )

        getNext thisActor ( lastActor, idx ) moreScript =
            case moreScript of
                ( newActor, newTxt ) :: rest ->
                    let
                        newIdx =
                            if newActor == lastActor then
                                idx + 1

                            else
                                idx + 1.75
                    in
                    if newActor == thisActor then
                        ( mkTxt newIdx newTxt, newIdx ) :: getNext thisActor ( newActor, newIdx ) rest

                    else
                        getNext thisActor ( newActor, newIdx ) rest

                [] ->
                    []

        shapesAndVisibility =
            case theScript of
                ( newActor, newTxt ) :: rest ->
                    getNext actor ( newActor, 0.5 ) theScript

                [] ->
                    []
    in
    ( group <| List.map Tuple.first shapesAndVisibility
    , List.map Tuple.second shapesAndVisibility
    )


scriptSpeed =
    textHeight


type Msg
    = Tick Float GetKeyState
    | HideInstructions
    | Restart1
    | Restart2
    | Restart2end
    | Restart3
    | Restart4
    | Restart4half
    | Restart4end
    | Restart5
    | Restartend
    | WelcomeTo1
    | T1to2
    | T1to3
    | T2toend
    | T2to3
    | T3to4
    | T3to5
    | T4toend
    | T4to4half
    | T4halfto5
    | T5toFINALend
    | End2ToWelcome
    | End4ToWelcome


type State
    = Welcome Bool
    | Scene1
    | Scene2
    | Scene3
    | Scene2End
    | Scene4
    | Scene5
    | Scene4End
    | Scene4half
    | SceneEnd


update msg model =
    case msg of
        Tick t _ ->
            { model | time = t }

        HideInstructions ->
            case model.state of
                Welcome _ ->
                    { model
                        | state = Welcome False
                        , startTime = model.time
                    }

                otherwise ->
                    model

        WelcomeTo1 ->
            case model.state of
                Welcome _ ->
                    { model
                        | state = Scene1
                        , startTime = model.time
                    }

                otherwise ->
                    model

        Restart1 ->
            case model.state of
                Scene1 ->
                    { model | startTime = model.time }

                otherwise ->
                    model

        Restart2 ->
            case model.state of
                Scene2 ->
                    { model | startTime = model.time }

                otherwise ->
                    model

        Restart2end ->
            case model.state of
                Scene2End ->
                    { model | startTime = model.time }

                otherwise ->
                    model

        Restart3 ->
            case model.state of
                Scene3 ->
                    { model | startTime = model.time }

                otherwise ->
                    model

        Restart4 ->
            case model.state of
                Scene4 ->
                    { model | startTime = model.time }

                otherwise ->
                    model

        Restart4half ->
            case model.state of
                Scene4half ->
                    { model | startTime = model.time }

                otherwise ->
                    model

        Restart4end ->
            case model.state of
                Scene4End ->
                    { model | startTime = model.time }

                otherwise ->
                    model

        Restart5 ->
            case model.state of
                Scene5 ->
                    { model | startTime = model.time }

                otherwise ->
                    model

        Restartend ->
            case model.state of
                SceneEnd ->
                    { model | startTime = model.time }

                otherwise ->
                    model

        T1to2 ->
            case model.state of
                Scene1 ->
                    { model
                        | state = Scene2
                        , startTime = model.time
                    }

                otherwise ->
                    model

        T1to3 ->
            case model.state of
                Scene1 ->
                    { model
                        | state = Scene3
                        , startTime = model.time
                    }

                otherwise ->
                    model

        T2toend ->
            case model.state of
                Scene2 ->
                    { model
                        | state = Scene2End
                        , startTime = model.time
                    }

                otherwise ->
                    model

        T2to3 ->
            case model.state of
                Scene2 ->
                    { model
                        | state = Scene3
                        , startTime = model.time
                    }

                otherwise ->
                    model

        T3to4 ->
            case model.state of
                Scene3 ->
                    { model
                        | state = Scene4
                        , startTime = model.time
                    }

                otherwise ->
                    model

        T3to5 ->
            case model.state of
                Scene3 ->
                    { model
                        | state = Scene5
                        , startTime = model.time
                    }

                otherwise ->
                    model

        T4toend ->
            case model.state of
                Scene4 ->
                    { model
                        | state = Scene4End
                        , startTime = model.time
                    }

                otherwise ->
                    model

        T4to4half ->
            case model.state of
                Scene4 ->
                    { model
                        | state = Scene4half
                        , startTime = model.time
                    }

                otherwise ->
                    model

        T4halfto5 ->
            case model.state of
                Scene4half ->
                    { model | state = Scene5 }

                otherwise ->
                    model

        T5toFINALend ->
            case model.state of
                Scene5 ->
                    { model | state = SceneEnd }

                otherwise ->
                    model

        End4ToWelcome ->
            case model.state of
                Scene4End ->
                    { model | state = Welcome False }

                otherwise ->
                    model

        End2ToWelcome ->
            case model.state of
                Scene2End ->
                    { model | state = Welcome False }

                otherwise ->
                    model



--------------------------------GROUPS OF ELEMENTS---------------------------------------------------
--------------------------------REPLAY-------------------------------------------


buttonGreen =
    rgb 20 180 170


buttonHighlight t =
    rgba 255 255 170 (0.5 + 0.5 * sin (2 * t))


restartButton t =
    [ roundedRect 20 7 3
        |> outlined (solid 1.5) (buttonHighlight t)
    , text "Replay"
        |> size 6
        |> centered
        |> outlined (solid 1) (buttonHighlight t)
        |> move ( 0, -1.5 )
    , roundedRect 20 7 3
        |> filled (rgba 255 255 255 0.5)
        |> addOutline (solid 0.5) buttonGreen
    , text "Replay"
        |> size 6
        |> centered
        |> filled buttonGreen
        |> move ( 0, -1.5 )
    ]
        |> group



--------------------------------TURTLE------------------------------------------
--------------------------------PARTY HAT----------------------------------


partyHat colour1 colour2 colour3 colour4 colour5 colour6 =
    group
        [ --background colour of party hat (triangle portion)
          curve ( -51.25, 33.124 ) [ Pull ( -38.66, 4.3466 ) ( -26.07, -24.43 ), Pull ( -27.42, -26.07 ) ( -28.77, -27.72 ), Pull ( -53.05, -25.92 ) ( -77.33, -24.13 ), Pull ( -77.63, -22.63 ) ( -77.93, -21.13 ), Pull ( -67.29, 3.5971 ) ( -56.65, 28.327 ), Pull ( -55.3, 31.475 ) ( -53.95, 34.622 ), Pull ( -52.75, 33.873 ) ( -51.25, 33.124 ) ]
            |> filled colour1
            |> addOutline (solid 1) black

        -- tuff on the top
        , curve ( -52.45, 34.922 ) [ Pull ( -50.15, 29.875 ) ( -54.85, 28.627 ), Pull ( -58.75, 29.875 ) ( -56.65, 34.922 ), Pull ( -61.5, 32.272 ) ( -62.95, 36.421 ), Pull ( -63.6, 40.919 ) ( -55.45, 40.618 ), Pull ( -57.8, 42.416 ) ( -56.95, 45.414 ), Pull ( -52.05, 50.065 ) ( -50.36, 42.117 ), Pull ( -48.66, 45.116 ) ( -44.96, 42.716 ), Pull ( -43.86, 40.518 ) ( -46.16, 39.119 ), Pull ( -47.81, 38.07 ) ( -49.46, 37.62 ), Pull ( -46.56, 38.171 ) ( -46.46, 34.922 ), Pull ( -47.56, 32.473 ) ( -50.06, 33.423 ), Pull ( -51.4, 33.623 ) ( -52.45, 34.922 ) ]
            |> filled colour2
            |> addOutline (solid 1) black

        -- swirl at the top
        , curve ( -56.35, 41.217 ) [ Pull ( -61.25, 41.965 ) ( -61.75, 47.512 ), Pull ( -61.85, 50.462 ) ( -58.75, 49.611 ), Pull ( -57.25, 48.711 ) ( -58.15, 47.812 ), Pull ( -58.9, 47.362 ) ( -59.65, 47.512 ) ]
            |> outlined (solid 1) black

        -- circle at the left bottom (red in image)
        , curve ( -67.14, -22.63 ) [ Pull ( -64.99, -19.03 ) ( -66.24, -15.43 ), Pull ( -66.94, -10.24 ) ( -72.24, -9.442 ), Pull ( -74.34, -14.08 ) ( -76.44, -18.73 ), Pull ( -74.94, -21.58 ) ( -73.44, -24.43 ), Pull ( -70.29, -23.68 ) ( -67.14, -22.63 ) ]
            |> filled colour6
            |> addOutline (solid 1) black

        -- circle at the bottom (green in image)
        , curve ( -55.75, -21.13 ) [ Pull ( -56.7, -15.38 ) ( -51.85, -13.63 ), Pull ( -47.86, -12.38 ) ( -45.26, -15.73 ), Pull ( -43.86, -18.43 ) ( -45.26, -21.13 ), Pull ( -46.46, -23.83 ) ( -47.66, -26.52 ), Pull ( -51.55, -25.77 ) ( -55.45, -25.03 ), Pull ( -55.75, -23.08 ) ( -55.75, -21.13 ) ]
            |> filled colour4
            |> addOutline (solid 1) black

        -- tuff at the bottom
        , curve ( -28.47, -26.82 ) [ Pull ( -23.48, -20.18 ) ( -19.48, -24.13 ), Pull ( -16.13, -29.02 ) ( -20.98, -30.72 ), Pull ( -16.63, -30.97 ) ( -16.48, -35.22 ), Pull ( -15.38, -39.47 ) ( -23.08, -37.32 ), Pull ( -25.18, -35.82 ) ( -27.27, -34.32 ), Pull ( -24.52, -37.02 ) ( -26.97, -39.71 ), Pull ( -32.52, -41.52 ) ( -35.07, -36.72 ), Pull ( -34.42, -39.71 ) ( -37.17, -41.51 ), Pull ( -42.91, -42.01 ) ( -42.26, -36.12 ), Pull ( -43.31, -41.81 ) ( -48.56, -41.51 ), Pull ( -52.81, -41.57 ) ( -50.66, -34.02 ), Pull ( -52.3, -36.32 ) ( -53.95, -37.02 ), Pull ( -55.95, -35.92 ) ( -55.15, -33.42 ), Pull ( -55.15, -37.07 ) ( -58.75, -38.51 ), Pull ( -63.4, -39.07 ) ( -64.44, -34.02 ), Pull ( -65.94, -38.32 ) ( -69.84, -39.41 ), Pull ( -75.19, -39.02 ) ( -76.14, -34.02 ), Pull ( -78.13, -36.97 ) ( -81.53, -36.12 ), Pull ( -85.13, -34.72 ) ( -83.33, -29.52 ), Pull ( -82.63, -27.87 ) ( -80.93, -26.22 ), Pull ( -83.18, -27.07 ) ( -85.43, -25.33 ), Pull ( -87.28, -22.78 ) ( -84.53, -20.23 ), Pull ( -80.98, -19.03 ) ( -79.43, -19.63 ), Pull ( -77.08, -20.63 ) ( -76.14, -22.63 ), Pull ( -77.64, -20.23 ) ( -76.14, -17.83 ), Pull ( -74.64, -16.58 ) ( -73.14, -18.13 ), Pull ( -70.24, -20.33 ) ( -71.34, -23.53 ), Pull ( -70.59, -20.13 ) ( -66.84, -19.93 ), Pull ( -63.79, -19.23 ) ( -62.35, -22.33 ), Pull ( -61.5, -24.58 ) ( -61.45, -26.82 ), Pull ( -61.3, -22.88 ) ( -58.15, -21.73 ), Pull ( -55.45, -20.53 ) ( -52.75, -21.73 ), Pull ( -50.2, -24.13 ) ( -51.25, -26.52 ), Pull ( -49.41, -20.98 ) ( -44.36, -20.83 ), Pull ( -40.06, -21.38 ) ( -37.17, -27.12 ), Pull ( -37.17, -21.88 ) ( -33.57, -21.43 ), Pull ( -28.82, -21.43 ) ( -28.47, -26.82 ) ]
            |> filled colour2
            |> addOutline (solid 1) black

        -- circle at the left (green in image)
        , curve ( -65.94, 6.4449 ) [ Pull ( -65.6, 3.3964 ) ( -60.85, 2.548 ), Pull ( -55.15, 2.6463 ) ( -54.25, 6.7447 ), Pull ( -53.25, 10.042 ) ( -55.45, 13.339 ), Pull ( -58.75, 15.439 ) ( -61.45, 13.339 ), Pull ( -62.5, 12.589 ) ( -63.55, 13.039 ), Pull ( -64.89, 9.8922 ) ( -66.24, 6.7447 ) ]
            |> filled colour5
            |> addOutline (solid 1) black

        -- circle at the top right (dark blue in image)
        , curve ( -48.26, 25.929 ) [ Pull ( -54.96, 21.933 ) ( -51.85, 16.936 ), Pull ( -48.91, 12.638 ) ( -43.16, 13.339 ), Pull ( -45.56, 19.634 ) ( -48.26, 25.929 ) ]
            |> filled colour3
            |> addOutline (solid 1) black

        -- circle at the bottom right (orange in image)
        , curve ( -32.97, -8.543 ) [ Pull ( -38.02, -6.892 ) ( -39.86, -11.84 ), Pull ( -40.47, -15.13 ) ( -36.87, -17.23 ), Pull ( -34.02, -18.73 ) ( -31.17, -17.23 ), Pull ( -30.07, -16.88 ) ( -29.97, -15.13 ), Pull ( -31.47, -11.84 ) ( -32.97, -8.543 ) ]
            |> filled colour5
            |> addOutline (solid 1) black

        -- circle in middle (light blue in image)
        , curve ( -48.56, 6.1451 ) [ Pull ( -52.76, 3.7976 ) ( -51.55, -0.149 ), Pull ( -50.16, -3.648 ) ( -46.76, -3.747 ), Pull ( -42.36, -3.548 ) ( -41.36, 1.0491 ), Pull ( -41.31, 5.0473 ) ( -44.66, 5.8454 ), Pull ( -46.91, 6.9451 ) ( -48.56, 6.1451 ) ]
            |> filled colour4
            |> addOutline (solid 1) black

        -- circle in the middle (orange in image)
        , curve ( -58.45, -2.847 ) [ Pull ( -56.25, -3.496 ) ( -55.45, -6.744 ), Pull ( -55.45, -10.14 ) ( -59.65, -11.54 ), Pull ( -63.15, -11.64 ) ( -63.85, -9.142 ), Pull ( -64.95, -6.594 ) ( -62.65, -4.046 ), Pull ( -60.55, -2.447 ) ( -58.45, -2.847 ) ]
            |> filled colour3
            |> addOutline (solid 1) black
        ]



---------------------------------------------------------------------------
-- Your shapes go here!


addTurtle ( x, y ) ( u, v ) =
    ( x + u, y + v )


subTurtle ( x, y ) ( u, v ) =
    ( x - u, y - v )


scaleTurtle s ( x, y ) =
    ( s * x, s * y )


turtle model =
    group
        [ curve ( -46.69, 24.698 ) [ Pull ( -47.59, 23.797 ) ( -48.49, 22.895 ), Pull ( -49.03, 22.535 ) ( -49.57, 22.174 ), Pull ( -50.29, 22.174 ) ( -51.01, 22.174 ), Pull ( -51.74, 22.174 ) ( -52.46, 22.174 ), Pull ( -53.54, 22.354 ) ( -54.62, 22.535 ), Pull ( -55.88, 23.076 ) ( -57.14, 23.616 ), Pull ( -59.49, 24.157 ) ( -61.83, 24.698 ), Pull ( -64, 24.878 ) ( -66.16, 25.059 ), Pull ( -69.76, 24.518 ) ( -73.37, 23.977 ), Pull ( -74.45, 23.616 ) ( -75.53, 23.256 ), Pull ( -76.8, 22.535 ) ( -78.06, 21.814 ), Pull ( -79.14, 20.552 ) ( -80.22, 19.29 ), Pull ( -80.58, 18.388 ) ( -80.94, 17.487 ), Pull ( -81.3, 15.143 ) ( -81.66, 12.8 ), Pull ( -82.2, 12.078 ) ( -82.74, 11.357 ), Pull ( -83.1, 10.636 ) ( -83.47, 9.9154 ), Pull ( -83.47, 8.1126 ) ( -83.47, 6.3098 ), Pull ( -83.1, 6.1295 ) ( -82.74, 5.9492 ), Pull ( -81.66, 5.9492 ) ( -80.58, 5.9492 ), Pull ( -79.68, 5.9492 ) ( -78.78, 5.9492 ), Pull ( -80.76, 5.769 ) ( -82.74, 5.5887 ), Pull ( -82.2, 4.6873 ) ( -81.66, 3.7859 ), Pull ( -81.12, 3.245 ) ( -80.58, 2.7042 ), Pull ( -79.68, 2.3436 ) ( -78.78, 1.983 ), Pull ( -77.34, 1.983 ) ( -75.89, 1.983 ), Pull ( -73.37, 2.1633 ) ( -70.85, 2.3436 ), Pull ( -69.76, 2.5239 ) ( -68.68, 2.7042 ), Pull ( -67.78, 2.1633 ) ( -66.88, 1.6225 ), Pull ( -65.62, 1.6225 ) ( -64.36, 1.6225 ), Pull ( -62.91, 1.8028 ) ( -61.47, 1.983 ), Pull ( -59.31, 2.5239 ) ( -57.14, 3.0647 ), Pull ( -55.16, 3.245 ) ( -53.18, 3.4253 ), Pull ( -51.74, 3.4253 ) ( -50.29, 3.4253 ), Pull ( -48.13, 2.5239 ) ( -45.97, 1.6225 ), Pull ( -42.72, -0.36 ) ( -39.48, -2.343 ), Pull ( -32.99, 4.6873 ) ( -26.5, 11.718 ), Pull ( -33.35, 21.633 ) ( -40.2, 31.549 ), Pull ( -43.44, 28.123 ) ( -46.69, 24.698 ) ]
            |> filled darkGreen
        , circle 1
            |> filled black
            |> move ( -75, 15 )
        , curve ( -35.87, 3.7859 ) [ Pull ( -36.95, 2.1633 ) ( -38.03, 0.5408 ), Pull ( -39.3, -0.54 ) ( -40.56, -1.622 ), Pull ( -40.92, -1.983 ) ( -41.28, -2.343 ), Pull ( -38.03, -4.507 ) ( -34.79, -6.67 ), Pull ( -32.81, -7.752 ) ( -30.82, -8.833 ), Pull ( -29.38, -9.374 ) ( -27.94, -9.915 ), Pull ( -26.5, -10.45 ) ( -25.05, -10.99 ), Pull ( -23.25, -11.35 ) ( -21.45, -11.71 ), Pull ( -19.83, -12.07 ) ( -18.2, -12.43 ), Pull ( -15.5, -12.8 ) ( -12.8, -13.16 ), Pull ( -10.27, -13.16 ) ( -7.752, -13.16 ), Pull ( -5.047, -12.98 ) ( -2.343, -12.8 ), Pull ( 0.3605, -12.61 ) ( 3.0647, -12.43 ), Pull ( 5.4084, -12.07 ) ( 7.7521, -11.71 ), Pull ( 10.816, -11.17 ) ( 13.881, -10.63 ), Pull ( 16.405, -10.09 ) ( 18.929, -9.554 ), Pull ( 21.273, -9.014 ) ( 23.616, -8.473 ), Pull ( 26.321, -7.571 ) ( 29.025, -6.67 ), Pull ( 32.09, -5.408 ) ( 35.154, -4.146 ), Pull ( 41.645, 0.7211 ) ( 48.135, 5.5887 ), Pull ( 49.397, 6.8507 ) ( 50.659, 8.1126 ), Pull ( 51.56, 9.5549 ) ( 52.461, 10.997 ), Pull ( 8.2929, 7.5718 ) ( -35.87, 3.7859 ) ]
            |> filled brown
        , animationPieces
            (mkAnimationPieceVS addCurve
                subCurve
                scaleCurve
                flippersTurtle
            )
            (\t -> MyCurve ( 0, 0 ) [])
            (3 + 3 * cos model.time)
            |> drawCurve
            |> filled darkGreen
        , hindLimb model
        , curve ( -46.69, 25.419 ) [ Pull ( -46.33, 24.157 ) ( -45.97, 22.895 ), Pull ( -45.61, 21.633 ) ( -45.25, 20.371 ), Pull ( -44.52, 19.109 ) ( -43.8, 17.847 ), Pull ( -43.44, 16.405 ) ( -43.08, 14.963 ), Pull ( -42.36, 13.34 ) ( -41.64, 11.718 ), Pull ( -41.1, 10.095 ) ( -40.56, 8.4732 ), Pull ( -40.02, 7.7521 ) ( -39.48, 7.0309 ), Pull ( -38.58, 5.769 ) ( -37.67, 4.507 ), Pull ( -36.41, 3.4253 ) ( -35.15, 2.3436 ), Pull ( -33.71, 1.0816 ) ( -32.27, -0.18 ), Pull ( -31.18, -0.54 ) ( -30.1, -0.901 ), Pull ( -27.94, -1.442 ) ( -25.78, -1.983 ), Pull ( -23.25, -2.523 ) ( -20.73, -3.064 ), Pull ( -18.38, -3.785 ) ( -16.04, -4.507 ), Pull ( -13.16, -4.507 ) ( -10.27, -4.507 ), Pull ( -7.211, -4.867 ) ( -4.146, -5.228 ), Pull ( -1.442, -5.047 ) ( 1.2619, -4.867 ), Pull ( 4.3267, -3.966 ) ( 7.3915, -3.064 ), Pull ( 9.5549, -3.064 ) ( 11.718, -3.064 ), Pull ( 14.602, -2.704 ) ( 17.487, -2.343 ), Pull ( 39.301, 5.0478 ) ( 61.115, 12.439 ), Pull ( 50.839, 20.552 ) ( 40.563, 28.664 ), Pull ( 36.416, 31.188 ) ( 32.27, 33.712 ), Pull ( 29.205, 34.974 ) ( 26.14, 36.236 ), Pull ( 24.518, 36.777 ) ( 22.895, 37.318 ), Pull ( 20.912, 37.859 ) ( 18.929, 38.4 ), Pull ( 16.405, 39.301 ) ( 13.881, 40.202 ), Pull ( 9.5549, 40.923 ) ( 5.2281, 41.645 ), Pull ( 2.8845, 41.645 ) ( 0.5408, 41.645 ), Pull ( -1.802, 42.185 ) ( -4.146, 42.726 ), Pull ( -6.309, 43.087 ) ( -8.473, 43.447 ), Pull ( -12.79, 43.267 ) ( -17.12, 43.087 ), Pull ( -21.45, 42.185 ) ( -25.78, 41.284 ), Pull ( -27.94, 40.563 ) ( -30.1, 39.842 ), Pull ( -32.63, 38.039 ) ( -35.15, 36.236 ), Pull ( -38.4, 33.171 ) ( -41.64, 30.107 ), Pull ( -44.16, 27.943 ) ( -46.69, 25.419 ) ]
            |> filled darkBrown
        ]



-- this is called bilinear interpolation
-- it follows the line between two points, tracing out a polygon


mkAnimationPieceVS addVS subVS scaleVS listOfVectors =
    case listOfVectors of
        p0 :: p1 :: rest ->
            ( 1
            , \t ->
                addVS p0
                    (scaleVS
                        t
                        (subVS p1 p0)
                    )
            )
                :: mkAnimationPieceVS addVS subVS scaleVS (p1 :: rest)

        _ ->
            []


type MyCurve
    = MyCurve ( Float, Float ) (List Pull)


drawCurve (MyCurve p0 pulls) =
    curve p0 pulls


addPull (Pull p0 p1) (Pull p2 p3) =
    Pull (addTurtle p0 p2) (addTurtle p1 p3)


subPull (Pull p0 p1) (Pull p2 p3) =
    Pull (subTurtle p0 p2) (subTurtle p1 p3)


scalePull s (Pull p2 p3) =
    Pull (scaleTurtle s p2) (scaleTurtle s p3)


addCurve (MyCurve p0 pulls0) (MyCurve p1 pulls1) =
    MyCurve (addTurtle p0 p1) (List.map2 addPull pulls0 pulls1)


subCurve (MyCurve p0 pulls0) (MyCurve p1 pulls1) =
    MyCurve (subTurtle p0 p1) (List.map2 subPull pulls0 pulls1)


scaleCurve s (MyCurve p1 pulls1) =
    MyCurve (scaleTurtle s p1) (List.map (scalePull s) pulls1)


flippersTurtle =
    [ MyCurve ( -21.45, 2.7042 ) [ Pull ( -25.41, -4.687 ) ( -29.38, -12.07 ), Pull ( -29.56, -13.34 ) ( -29.74, -14.6 ), Pull ( -29.92, -15.32 ) ( -30.1, -16.04 ), Pull ( -29.74, -17.12 ) ( -29.38, -18.2 ), Pull ( -28.3, -20.55 ) ( -27.22, -22.89 ), Pull ( -24.87, -25.6 ) ( -22.53, -28.3 ), Pull ( -19.29, -30.82 ) ( -16.04, -33.35 ), Pull ( -9.915, -36.59 ) ( -3.785, -39.84 ), Pull ( 0.9014, -40.92 ) ( 5.5887, -42.0 ), Pull ( 7.0309, -41.64 ) ( 8.4732, -41.28 ), Pull ( 8.8338, -40.38 ) ( 9.1943, -39.48 ), Pull ( 8.6535, -38.21 ) ( 8.1126, -36.95 ), Pull ( 2.7042, -31.36 ) ( -2.704, -25.78 ), Pull ( -4.146, -23.61 ) ( -5.588, -21.45 ), Pull ( -7.03, -17.66 ) ( -8.473, -13.88 ), Pull ( -8.653, -11.53 ) ( -8.833, -9.194 ), Pull ( -7.211, -3.605 ) ( -5.588, 1.983 ), Pull ( -13.52, 2.3436 ) ( -21.45, 2.7042 ) ]
    , MyCurve ( -21.45, 2.7042 ) [ Pull ( -25.23, -4.507 ) ( -29.02, -11.71 ), Pull ( -29.38, -13.16 ) ( -29.74, -14.6 ), Pull ( -29.74, -16.22 ) ( -29.74, -17.84 ), Pull ( -28.3, -20.91 ) ( -26.86, -23.97 ), Pull ( -23.25, -28.12 ) ( -19.65, -32.27 ), Pull ( -13.88, -36.77 ) ( -8.112, -41.28 ), Pull ( -4.146, -43.44 ) ( -0.18, -45.61 ), Pull ( 0.9014, -45.79 ) ( 1.983, -45.97 ), Pull ( 2.7042, -45.79 ) ( 3.4253, -45.61 ), Pull ( 3.9661, -44.52 ) ( 4.507, -43.44 ), Pull ( 4.1464, -41.82 ) ( 3.7859, -40.2 ), Pull ( -0.901, -33.53 ) ( -5.588, -26.86 ), Pull ( -6.85, -24.33 ) ( -8.112, -21.81 ), Pull ( -8.653, -19.65 ) ( -9.194, -17.48 ), Pull ( -9.374, -14.24 ) ( -9.554, -10.99 ), Pull ( -8.653, -7.391 ) ( -7.752, -3.785 ), Pull ( -6.67, -0.901 ) ( -5.588, 1.983 ), Pull ( -13.52, 2.3436 ) ( -21.45, 2.7042 ) ]
    , MyCurve ( -21.45, 2.7042 ) [ Pull ( -25.05, -3.785 ) ( -28.66, -10.27 ), Pull ( -29.2, -12.98 ) ( -29.74, -15.68 ), Pull ( -29.74, -18.2 ) ( -29.74, -20.73 ), Pull ( -28.84, -24.51 ) ( -27.94, -28.3 ), Pull ( -26.32, -31.9 ) ( -24.69, -35.51 ), Pull ( -22.89, -37.85 ) ( -21.09, -40.2 ), Pull ( -18.02, -42.9 ) ( -14.96, -45.61 ), Pull ( -13.88, -46.33 ) ( -12.8, -47.05 ), Pull ( -11.89, -47.05 ) ( -10.99, -47.05 ), Pull ( -10.27, -45.97 ) ( -9.554, -44.89 ), Pull ( -9.554, -42.0 ) ( -9.554, -39.12 ), Pull ( -10.27, -35.15 ) ( -10.99, -31.18 ), Pull ( -11.89, -27.94 ) ( -12.8, -24.69 ), Pull ( -12.8, -20.73 ) ( -12.8, -16.76 ), Pull ( -12.43, -13.34 ) ( -12.07, -9.915 ), Pull ( -10.81, -6.67 ) ( -9.554, -3.425 ), Pull ( -7.932, -0.721 ) ( -6.309, 1.983 ), Pull ( -13.88, 2.3436 ) ( -21.45, 2.7042 ) ]
    , MyCurve ( -21.45, 2.7042 ) [ Pull ( -25.23, -4.326 ) ( -29.02, -11.35 ), Pull ( -29.56, -12.61 ) ( -30.1, -13.88 ), Pull ( -30.46, -15.32 ) ( -30.82, -16.76 ), Pull ( -31.18, -18.92 ) ( -31.54, -21.09 ), Pull ( -31.18, -25.96 ) ( -30.82, -30.82 ), Pull ( -29.92, -35.33 ) ( -29.02, -39.84 ), Pull ( -27.94, -43.26 ) ( -26.86, -46.69 ), Pull ( -26.14, -47.41 ) ( -25.41, -48.13 ), Pull ( -24.51, -48.31 ) ( -23.61, -48.49 ), Pull ( -22.53, -47.77 ) ( -21.45, -47.05 ), Pull ( -20.91, -45.25 ) ( -20.37, -43.44 ), Pull ( -19.83, -40.56 ) ( -19.29, -37.67 ), Pull ( -18.92, -35.87 ) ( -18.56, -34.07 ), Pull ( -17.84, -31.36 ) ( -17.12, -28.66 ), Pull ( -16.58, -26.14 ) ( -16.04, -23.61 ), Pull ( -15.32, -21.09 ) ( -14.6, -18.56 ), Pull ( -10.63, -8.292 ) ( -6.67, 1.983 ), Pull ( -14.06, 2.3436 ) ( -21.45, 2.7042 ) ]
    , MyCurve ( -21.81, 2.7042 ) [ Pull ( -25.78, -4.507 ) ( -29.74, -11.71 ), Pull ( -30.46, -13.52 ) ( -31.18, -15.32 ), Pull ( -32.09, -17.66 ) ( -32.99, -20.01 ), Pull ( -33.53, -22.35 ) ( -34.07, -24.69 ), Pull ( -34.61, -27.76 ) ( -35.15, -30.82 ), Pull ( -35.51, -34.43 ) ( -35.87, -38.03 ), Pull ( -35.51, -41.82 ) ( -35.15, -45.61 ), Pull ( -34.43, -46.33 ) ( -33.71, -47.05 ), Pull ( -32.63, -47.05 ) ( -31.54, -47.05 ), Pull ( -30.46, -45.97 ) ( -29.38, -44.89 ), Pull ( -27.94, -42.54 ) ( -26.5, -40.2 ), Pull ( -25.23, -37.13 ) ( -23.97, -34.07 ), Pull ( -22.53, -30.82 ) ( -21.09, -27.58 ), Pull ( -19.83, -24.69 ) ( -18.56, -21.81 ), Pull ( -17.12, -18.92 ) ( -15.68, -16.04 ), Pull ( -14.6, -13.88 ) ( -13.52, -11.71 ), Pull ( -10.45, -5.588 ) ( -7.391, 0.5408 ), Pull ( -14.78, 1.6225 ) ( -21.81, 2.7042 ) ]
    , MyCurve ( -22.17, 2.7042 ) [ Pull ( -24.69, -1.983 ) ( -27.22, -6.67 ), Pull ( -28.66, -9.554 ) ( -30.1, -12.43 ), Pull ( -32.09, -16.4 ) ( -34.07, -20.37 ), Pull ( -35.87, -24.51 ) ( -37.67, -28.66 ), Pull ( -38.76, -31.0 ) ( -39.84, -33.35 ), Pull ( -40.56, -35.15 ) ( -41.28, -36.95 ), Pull ( -41.64, -39.12 ) ( -42.0, -41.28 ), Pull ( -42.0, -42.54 ) ( -42.0, -43.8 ), Pull ( -41.28, -44.34 ) ( -40.56, -44.89 ), Pull ( -39.3, -44.52 ) ( -38.03, -44.16 ), Pull ( -36.59, -43.08 ) ( -35.15, -42.0 ), Pull ( -32.99, -39.84 ) ( -30.82, -37.67 ), Pull ( -28.48, -34.79 ) ( -26.14, -31.9 ), Pull ( -24.51, -29.74 ) ( -22.89, -27.58 ), Pull ( -20.73, -23.97 ) ( -18.56, -20.37 ), Pull ( -16.04, -15.68 ) ( -13.52, -10.99 ), Pull ( -10.45, -5.228 ) ( -7.391, 0.5408 ), Pull ( -14.78, 1.6225 ) ( -22.17, 2.7042 ) ]
    , MyCurve ( -22.53, 2.7042 ) [ Pull ( -27.22, -5.408 ) ( -31.9, -13.52 ), Pull ( -33.53, -15.86 ) ( -35.15, -18.2 ), Pull ( -37.67, -21.45 ) ( -40.2, -24.69 ), Pull ( -42.0, -26.86 ) ( -43.8, -29.02 ), Pull ( -45.61, -30.64 ) ( -47.41, -32.27 ), Pull ( -48.67, -33.17 ) ( -49.93, -34.07 ), Pull ( -50.47, -35.15 ) ( -51.01, -36.23 ), Pull ( -51.38, -36.95 ) ( -51.74, -37.67 ), Pull ( -51.74, -38.76 ) ( -51.74, -39.84 ), Pull ( -51.38, -40.2 ) ( -51.01, -40.56 ), Pull ( -49.93, -40.92 ) ( -48.85, -41.28 ), Pull ( -47.05, -41.1 ) ( -45.25, -40.92 ), Pull ( -42.36, -40.02 ) ( -39.48, -39.12 ), Pull ( -35.51, -36.59 ) ( -31.54, -34.07 ), Pull ( -27.22, -29.56 ) ( -22.89, -25.05 ), Pull ( -17.66, -17.3 ) ( -12.43, -9.554 ), Pull ( -9.735, -4.507 ) ( -7.03, 0.5408 ), Pull ( -14.78, 1.6225 ) ( -22.53, 2.7042 ) ]
    ]



-- needs at least 3 floats to interpolate


interpolateFloats floats t =
    let
        len =
            Array.length floats

        intTime =
            floor t

        fracTime =
            0.5 * pi * (t - toFloat intTime)

        idx0 =
            modBy len intTime

        idx1 =
            modBy len (intTime + 1)

        idx2 =
            modBy len (intTime + 2)

        idx3 =
            modBy len (intTime + 3)
    in
    case ( Array.get idx0 floats, Array.get idx1 floats, Array.get idx2 floats ) of
        ( Just p0, Just p1, Just p2 ) ->
            let
                p0top2 =
                    p2 - p0

                midPoint =
                    0.5 * (p2 + p0)

                midTop1 =
                    p1 - midPoint
            in
            sin fracTime

        _ ->
            0


hindLimb model =
    group
        [ curve ( 29.025, 4.507 ) [ Pull ( 31.729, 0.3605 ) ( 34.433, -3.785 ), Pull ( 35.695, -4.867 ) ( 36.957, -5.949 ), Pull ( 38.4, -6.67 ) ( 39.842, -7.391 ), Pull ( 42.005, -8.473 ) ( 44.169, -9.554 ), Pull ( 47.053, -10.81 ) ( 49.938, -12.07 ), Pull ( 52.822, -13.16 ) ( 55.707, -14.24 ), Pull ( 58.591, -15.32 ) ( 61.476, -16.4 ), Pull ( 64, -17.66 ) ( 66.523, -18.92 ), Pull ( 67.966, -19.83 ) ( 69.408, -20.73 ), Pull ( 70.129, -20.55 ) ( 70.85, -20.37 ), Pull ( 71.571, -19.83 ) ( 72.292, -19.29 ), Pull ( 72.653, -17.3 ) ( 73.014, -15.32 ), Pull ( 73.014, -13.52 ) ( 73.014, -11.71 ), Pull ( 72.292, -9.915 ) ( 71.571, -8.112 ), Pull ( 71.211, -6.67 ) ( 70.85, -5.228 ), Pull ( 70.129, -3.785 ) ( 69.408, -2.343 ), Pull ( 68.687, -1.802 ) ( 67.966, -1.261 ), Pull ( 66.523, 0 ) ( 65.081, 1.2619 ), Pull ( 64, 2.7042 ) ( 62.918, 4.1464 ), Pull ( 61.836, 4.6873 ) ( 60.754, 5.2281 ), Pull ( 59.673, 5.0478 ) ( 58.591, 4.8676 ), Pull ( 57.149, 4.6873 ) ( 55.707, 4.507 ), Pull ( 53.904, 4.6873 ) ( 52.101, 4.8676 ), Pull ( 49.938, 5.5887 ) ( 47.774, 6.3098 ), Pull ( 44.89, 8.1126 ) ( 42.005, 9.9154 ), Pull ( 35.515, 7.2112 ) ( 29.025, 4.507 ) ]
            |> filled darkGreen
            |> rotate (degrees (2 * sin model.time))
        ]



--------------------------------GENERAL BACKGROUND SCENE ELEMENTS---------------------


backgroundElements brightness schoolOfFish model =
    group
        [ water brightness
        , schoolOfFish model
        , turtle model
            |> scale 0.3
            |> move ( repeatDuration -3 500 100 model.time, 35 )

        --seaweed
        , let
            seaweed idx rawThickness =
                let
                    thickness =
                        2 + rawThickness

                    spotHeight =
                        sin (17 * toFloat idx)

                    spotRot =
                        -1 * cos (17 * toFloat idx)

                    waveTime =
                        model.time + 0.1 * toFloat idx

                    wholeSeaweed =
                        curve ( -thickness, -50 )
                            [ Pull ( -thickness + 10 * cos waveTime, -25 )
                                ( -thickness, 0 )
                            , Pull ( -thickness - 10 * cos waveTime, 25 ) ( 0, 50 )
                            , Pull ( 0, 50 ) ( 0, 50 )
                            , Pull ( thickness - 10 * cos waveTime, 25 ) ( thickness, 0 )
                            , Pull ( thickness + 10 * cos waveTime, -25 ) ( thickness, -50 )
                            , Pull ( 0, -50 ) ( -thickness, -50 )
                            ]
                            |> filled (hsl (degrees 92) 0.722 0.408)
                in
                [ wholeSeaweed
                , oval 3 7
                    |> filled (hsl (degrees 59) 0.437 0.463)
                    |> rotate (0.35 * cos waveTime * spotRot)
                    |> move ( spotHeight * (0.5 * thickness + 5 * cos waveTime), -25 * spotHeight )
                    |> clip wholeSeaweed
                ]
                    |> group
          in
          List.map2 (\idx thickness -> seaweed idx thickness |> move ( 10 * toFloat idx, 0 ))
            (List.range -10 10)
            [ 0.85, 0.095, 0.295, 0.231, 0.615, 0.428, 0.26, 0.31, 0.173, 0.397, 0.725, 0.44, 0.104, 0.156, 0.073, 0.47, 0.627, 0.22, 0.726, 0.847 ]
            |> group
            |> move ( 0, -30 )
            |> scale 1.2

        -- sand
        , oval 500 100
            |> filled
                (gradient
                    [ stop (hsl (degrees 43) 0.53 0.61) 0
                    , stop (hsl (degrees 43) 0.714 0.778) 25
                    ]
                )
            |> move ( -35, -90 )

        --seaweed behind rock
        , seaweedRock |> move ( -45, -38 )
        , ngon 12 14
            -- rock
            |> filled gray
            |> scaleX 2
            |> addOutline (solid 0.4) black
            |> move ( -75, -35 )
        , ngon 14 11
            -- rock
            |> filled gray
            |> scaleX 2
            |> addOutline (solid 0.4) black
            |> move ( -60, -42 )
        , treasureChess
            |> move ( 75, -40 )
        , seaweedRock |> move ( 45, -45 )
        , seaweedRock |> move ( 60, -62 )
        , seaweedRock |> move ( -70, -57 )
        ]



--------------------------------PARROT FISH (PIXAR)-------------------------------------------


parrotFish model =
    group
        [ oval 40 17
            -- top fin
            |> filled
                (gradient
                    [ stop (hsl (degrees 256) 0.298 0.57) 0
                    , stop (hsl (degrees 31) 0.975 0.57) 50
                    ]
                )
            |> rotate (degrees -5)
            |> move ( 0, 13 )
        , oval 30 12
            -- bottom fin
            |> filled
                (gradient
                    [ stop (hsl (degrees 256) 0.298 0.57) 0
                    , stop (hsl (degrees 31) 0.975 0.57) 50
                    ]
                )
            |> rotate (degrees 8)
            |> move ( -5, -13 )
        , -- body
          square 100
            |> filled
                (gradient
                    [ stop (hsl (degrees 160) 0.619 0.53) 0
                    , stop (hsl (degrees 31) 0.975 0.57) 25
                    , stop (hsl (degrees 52) 0.975 0.57) 30
                    ]
                )
            |> rotate (degrees -90)
            |> clip
                ([ oval 34 60
                    |> ghost
                    |> rotate (degrees -90)
                 , oval 20 12
                    -- tail
                    |> filled black
                    |> rotate (degrees -20)
                    |> move ( -32, 3 )
                    |> rotate (degrees 2 * sin (2 * model.time))
                 , oval 20 12
                    -- tail
                    |> filled black
                    |> rotate (degrees 20)
                    |> move ( -32, -3 )
                    |> rotate (degrees 2 * sin (2 * model.time))
                 ]
                    |> group
                )
            |> subtract (wedge 4 0.5 |> ghost |> rotate (degrees 90) |> move ( 28, -3.5 ))
            |> subtract (wedge 3.5 0.5 |> ghost |> rotate (degrees -90) |> move ( 27.5, -3 ))
        , -- mouth
          [ wedge 4 0.5
                |> filled (hsl (degrees 256) 0.5 0.6)
                |> rotate (degrees 90)
                |> scaleY 1.3
                |> move ( 5, 0 )
                |> rotate (degrees (abs (10 * sin model.time)))
          , wedge 3.5 0.5
                |> filled (hsl (degrees 256) 0.5 0.6)
                |> rotate (degrees 90)
                |> move ( 4.5, -0.5 )
                |> rotate (degrees (abs (10 * sin model.time)))
                |> mirrorY
          ]
            |> group
            |> move ( 23, -3.5 )
        , -- eye
          oval 8 10
            |> filled white
            |> move ( 18, 5 )
        , oval 5 6
            |> filled black
            |> move ( 19, 5 )
        , circle 1
            |> filled white
            |> move ( 18, 6 )
        ]



--------------------------------FISH BACKGROUND----------------------------------


fishAni model =
    group
        [ fishBackground
            |> move ( repeatDuration 9 25 -105 model.time, -10 )
        , fishBackground
            |> scale 0.8
            |> mirrorX
            |> move ( repeatDuration -10 23 105 model.time, 0 )
        , fishBackground
            |> scale 0.6
            |> move ( repeatDuration 7 35 -120 model.time, 10 )
        , fishBackground
            |> scale 0.6
            |> mirrorX
            |> move ( repeatDuration -7 35 130 model.time, 25 )
        , fishBackground
            |> scale 0.6
            |> mirrorX
            |> move ( repeatDuration -10 25 -105 model.time, 40 )
        ]


fishBackground =
    group
        [ oval 15 10
            |> filled (hsl (degrees 226) 0.2 0.55)
        , triangle 5
            |> filled (hsl (degrees 226) 0.2 0.55)
            |> move ( -8, 0 )
        ]



--------------------------------TREASURE CHEST--------------------------------------


treasureChess =
    group
        [ rect 24 15
            -- treasure chess
            |> filled (hsl (degrees 32) 0.568 0.4)
            |> addOutline (solid 1) black
        , rect 24 5
            |> filled (hsl (degrees 32) 0.568 0.4)
            |> addOutline (solid 0.8) black
        , wedge 12 0.5
            |> filled (hsl (degrees 32) 0.568 0.4)
            |> addOutline (solid 1) black
            |> rotate (degrees 90)
            |> move ( 0, 7.5 )
        , rect 3.5 3
            |> filled yellow
            |> addOutline (solid 0.5) black
            |> move ( -11, 7 )
        ]



--------------------------------ROCK SEAWEED--------------------------------------


seaweedRock =
    group
        [ oval 2 25
            |> filled lightGreen
            |> move ( 0, 12 )
        , oval 2 20
            |> filled lightGreen
            |> move ( 0, 10 )
            |> rotate (degrees 20)
        , oval 2 20
            |> filled lightGreen
            |> move ( 0, 10 )
            |> rotate (degrees -20)
        ]



-------------------------------


textBlue =
    rgb 45 8 255



--------------------------------JELLYFISH-------------------------------------------------------
{- myShapes model =
   [
     jellyfish (hsl (degrees 30*model.time) 1 0.5)
   ]
-}


jellyfish color =
    group
        [ tentacles color
        , wedge 20 0.5
            |> filled color
            |> rotate (degrees 90)
        , circle 2
            |> filled black
            |> move ( -10, 8 )
        , circle 2
            |> filled black
            |> move ( 10, 8 )
        , circle 0.5
            |> filled white
            |> move ( -9, 8 )
        , circle 0.5
            |> filled white
            |> move ( 11, 8 )
        , wedge 3 0.5
            |> filled darkRed
            |> rotate (degrees -90)
            |> move ( 0, 5 )
        ]



-- to flip your shape/object |> scaleX -1


tentacles color =
    group
        [ -- tentacle 1
          curve ( 0, 0 ) [ Pull ( 10, 0 ) ( 20, -10 ) ]
            |> filled color
            |> rotate (degrees -60)
            |> move ( -12, 1.5 )
        , curve ( 0, 0 ) [ Pull ( 10, 0 ) ( 20, -10 ) ]
            |> filled color
            |> rotate (degrees 120)
            |> move ( -9, -40 )
        , -- tentacle 2
          curve ( 0, 0 ) [ Pull ( 10, 0 ) ( 20, -10 ) ]
            |> filled color
            |> rotate (degrees -60)
            |> scale 0.75
            |> move ( -6, 1.5 )
        , curve ( 0, 0 ) [ Pull ( 10, 0 ) ( 20, -10 ) ]
            |> filled color
            |> rotate (degrees 120)
            |> scale 0.75
            |> move ( -4, -30 )
        , -- tentacle 3
          curve ( 0, 0 ) [ Pull ( 10, 0 ) ( 20, -10 ) ]
            |> filled color
            |> rotate (degrees -60)
            |> move ( 0, 1.5 )
        , curve ( 0, 0 ) [ Pull ( 10, 0 ) ( 20, -10 ) ]
            |> filled color
            |> rotate (degrees 120)
            |> move ( 3, -40 )
        , -- tentacle 4
          curve ( 0, 0 ) [ Pull ( 10, 0 ) ( 20, -10 ) ]
            |> filled color
            |> rotate (degrees -60)
            |> scale 0.75
            |> move ( 6, 1.5 )
        , curve ( 0, 0 ) [ Pull ( 10, 0 ) ( 20, -10 ) ]
            |> filled color
            |> rotate (degrees 120)
            |> scale 0.75
            |> move ( 8, -30 )
        , -- tentacle 5
          curve ( 0, 0 ) [ Pull ( 10, 0 ) ( 20, -10 ) ]
            |> filled color
            |> rotate (degrees -60)
            |> move ( 12, 1.5 )
        , curve ( 0, 0 ) [ Pull ( 10, 0 ) ( 20, -10 ) ]
            |> filled color
            |> rotate (degrees 120)
            |> move ( 15, -40 )
        ]



-----------------------------------------------------------------------------------------------------
--------------------------------CORAL-------------------------------------------------------------------
{- myShapes model =
   [
     yellowCoralTotal blue 6
      |> scale 0.8
      |> move (-10, 15)
   ]
-}


yellowCoralTotal coralColour num =
    group
        [ yellowCoral coralColour num
        ]


yellowCoral coralColour size =
    (polygon
        [ ( 0.25 * size, size * 0.5 )
        , ( 0.25 * size, 0.5 * size )
        , ( 0.25 * size, 2.5 * size )
        , ( 0.25 * size, 4.5 * size )
        , ( 0, 5.5 * size )
        , ( -0.25 * size, 4.5 * size )
        , ( -0.25 * size, 3.5 * size )
        , ( -0.25 * size, 0.5 * size )
        , ( -0.25 * size, size * 0.5 )
        ]
        |> filled coralColour
        --(rgb (80*size+200) (80*size+200) 0)
        |> scaleY 1.4
    )
        :: (if size > 0.5 then
                [ yellowCoral coralColour (0.5 * size) |> rotate (degrees -60) |> move ( -0.8, size * 1.7 )
                , yellowCoral coralColour (0.4 * size) |> rotate (degrees -55) |> move ( 0, size * 4 )
                , yellowCoral coralColour (0.5 * size) |> rotate (degrees 60) |> move ( 0.6, size * 1.5 )
                , yellowCoral coralColour (0.4 * size) |> rotate (degrees 55) |> move ( 0.4, size * 4.5 )
                , yellowCoral coralColour (0.3 * size) |> rotate (degrees 45) |> move ( 0.3, size * 6.8 )
                , yellowCoral coralColour (0.3 * size) |> rotate (degrees -60) |> move ( 0.4, size * 6.2 )
                ]

            else
                []
           )
        |> group



------------------------------------------------------------------------------------------------
--------------------------------SCULPTURES---------------------------------------------
------SAND SCULPTURES---------------------------------------------------
-- pufferfish


pufferfishS =
    group
        [ -- top spikes
          spikeS
            |> move ( 0, 13 )
        , spikeS
            |> rotate (degrees 40)
            |> move ( -11, 9 )
        , spikeS
            |> rotate (degrees -40)
            |> move ( 11, 9 )
        , -- bottom spikes
          spikeS
            |> scaleY -1
            |> move ( 0, -13 )
        , spikeS
            |> rotate (degrees 40)
            |> scaleY -1
            |> move ( -11, -9 )
        , spikeS
            |> rotate (degrees -40)
            |> scaleY -1
            |> move ( 11, -9 )
        , tailS
            |> rotate (degrees 90)
            |> move ( -20, 0 )
        , oval 30 25
            |> filled (sandGradient 0 50)
            |> addOutline (solid 0.5) black
        , -- spikes on the body
          spikeS
            |> rotate (degrees 20)
            |> move ( -5, 7 )
        , spikeS
            |> rotate (degrees -20)
            |> move ( 5, 7 )
        , spikeS
            |> rotate (degrees 20)
            |> scaleY -1
            |> move ( -5, -7 )
        , spikeS
            |> rotate (degrees -20)
            |> scaleY -1
            |> move ( 5, -7 )
        , -- eyes
          eyeShellS
            |> scale 0.8
            |> move ( -6, 12 )
        , eyeShellS
            |> scale 0.8
            |> scaleX -1
            |> move ( 16, 12 )
        , -- smile
          mouthShellS
            |> scale 0.15
            |> move ( 0, -2.5 )
        , -- fins
          wedge 7 0.25
            |> filled (sandGradient -5 20 |> rotateGradient (degrees 120))
            |> rotate (degrees -180)
            |> subtract (rect 4 10 |> filled white |> move ( 1, 0 ))
            |> move ( -7, 0 )
        , wedge 7 0.25
            |> filled (sandGradient 0 10)
            |> subtract (rect 4 10 |> filled white)
            |> move ( 12.5, 0 )
        ]


spikeS =
    triangle 5
        |> filled (sandGradient 0 10)
        |> rotate (degrees -30)
        |> subtract (rect 9 2.5 |> filled white |> move ( 0, -2 ))


tailS =
    group
        [ triangle 7
            |> filled (sandGradient 0 50)
            |> rotate (degrees 30)
            |> move ( 0, -5 )
        , circle 3.5
            |> filled (sandGradient 0 50 |> rotateGradient (degrees 180))
            |> move ( -3, 0 )
        , circle 3.5
            |> filled (sandGradient 0 50 |> rotateGradient (degrees 180))
            |> move ( 3, 0 )
        ]


sandGradient x0 x1 =
    gradient
        [ stop (hsl (degrees 26) 0.93 0.25) x0
        , stop (hsl (degrees 42) 0.5 0.75) x1
        ]
        |> rotateGradient (degrees 33)



-- parrotfish


parrotFishS model =
    group
        [ oval 40 17
            -- top fin
            |> filled (sandGradient 10 70)
            |> rotate (degrees -5)
            |> move ( 0, 13 )
        , oval 30 12
            -- bottom fin
            |> filled (sandGradient -15 5)
            |> rotate (degrees 8)
            |> move ( -5, -13 )
        , oval 20 12
            -- tail
            |> filled (sandGradient 0 20 |> rotateGradient (degrees -30))
            |> rotate (degrees -20)
            |> move ( -32, 3 )
            |> rotate (degrees 2 * sin (2 * model.time))
        , oval 20 12
            -- tail
            |> filled (sandGradient 0 20 |> rotateGradient (degrees -90))
            |> rotate (degrees 20)
            |> move ( -32, -3 )
            |> rotate (degrees 2 * sin (2 * model.time))

        -- body
        , oval 34 60
            |> filled (sandGradient 0 45)
            |> rotate (degrees -90)
        , -- mouth
          mouthShellS
            |> scale 0.4
            |> move ( 15, -2 )
        , eyeShellS
            |> move ( 5, 17.5 )
        ]


eyeShellS =
    curve ( 24.284, -35.28 )
        [ Pull ( 25.325, -36.84 ) ( 23.566, -37.2 )
        , Pull ( 22.387, -37.0 ) ( 22.609, -34.81 )
        , Pull ( 23.225, -33.33 ) ( 25.241, -33.85 )
        , Pull ( 27.858, -35.74 ) ( 26.676, -38.63 )
        , Pull ( 25.044, -41.27 ) ( 21.413, -40.31 )
        , Pull ( 15.375, -37.47 ) ( 19.738, -30.02 )
        , Pull ( 24.119, -24.62 ) ( 31.7, -30.02 )
        , Pull ( 36.857, -34.98 ) ( 33.614, -41.74 )
        , Pull ( 28.401, -50.8 ) ( 16.388, -44.85 )
        , Pull ( 8.2514, -34.27 ) ( 14.714, -25.48 )
        , Pull ( 23.201, -16.4 ) ( 35.289, -20.93 )
        , Pull ( 45.692, -25.58 ) ( 46.295, -38.63 )
        , Pull ( 48.156, -41.03 ) ( 45.816, -43.42 )
        , Pull ( 38.041, -49.44 ) ( 30.265, -46.05 )
        , Pull ( 30.145, -45.93 ) ( 30.026, -45.81 )
        ]
        |> filled (hsl (degrees 186) 0.802 0.373)
        |> addOutline (solid 2) (hsl (degrees 227) 0.973 0.455)
        |> scale 0.3


mouthShellS =
    [ curve ( 10.791, 1.6487 ) [ Pull ( 9.4861, 11.99 ) ( 24.58, 22.332 ), Pull ( 30.775, 26.43 ) ( 38.37, 28.327 ), Pull ( 41.968, 29.028 ) ( 42.566, 25.929 ), Pull ( 42.571, 17.935 ) ( 29.976, 10.941 ), Pull ( 24.23, 7.4941 ) ( 20.084, 4.0468 ), Pull ( 15.437, 1.7978 ) ( 10.791, 1.6487 ) ]
        |> filled (rgb 117 131 174)
        |> addOutline (solid 0.5) (rgb 53 50 98)
        |> rotate (degrees -15)
    , curve ( 19.784, 3.4473 ) [ Pull ( 30.725, 5.2967 ) ( 43.466, 4.9461 ), Pull ( 47.512, 4.9964 ) ( 51.559, 4.0468 ), Pull ( 53.161, 2.4482 ) ( 46.763, 0.4496 ), Pull ( 36.222, -2.499 ) ( 23.681, -2.248 ), Pull ( 16.086, -1.748 ) ( 11.69, -0.449 ), Pull ( 10.191, 0.4496 ) ( 11.091, 1.3489 ), Pull ( 15.288, 2.3981 ) ( 19.784, 3.4473 ) ]
        |> filled (rgb 117 131 174)
        |> addOutline (solid 0.5) (rgb 53 50 98)
        |> rotate (degrees -10)
    , --decorations on the shell (top)
      curve ( 25.779, 22.632 ) [ Pull ( 24.731, 19.334 ) ( 20.683, 16.037 ), Pull ( 17.386, 13.738 ) ( 14.088, 13.039 ) ]
        |> outlined (solid 0.25) (rgb 53 50 98)
        |> rotate (degrees -15)
    , curve ( 13.189, 10.042 ) [ Pull ( 27.633, 14.936 ) ( 30.276, 25.03 ) ]
        |> outlined (solid 0.25) (rgb 53 50 98)
        |> rotate (degrees -15)
    , curve ( 34.173, 26.529 ) [ Pull ( 33.825, 20.533 ) ( 27.878, 14.538 ), Pull ( 22.782, 9.5908 ) ( 16.487, 8.843 ), Pull ( 13.438, 8.2435 ) ( 11.99, 7.644 ) ]
        |> outlined (solid 0.25) (rgb 53 50 98)
        |> rotate (degrees -15)
    , curve ( 10.791, 4.3466 ) [ Pull ( 23.182, 6.2918 ) ( 33.573, 16.637 ), Pull ( 38.872, 22.182 ) ( 37.17, 27.728 ) ]
        |> outlined (solid 0.5) (rgb 53 50 98)
        |> rotate (degrees -15)
    , --decorations on the shell (bottom)
      curve ( 13.789, 1.9484 ) [ Pull ( 30.875, -3.252 ) ( 47.962, 4.9461 ) ]
        |> outlined (solid 0.5) (rgb 53 50 98)
        |> rotate (degrees -10)
    , curve ( 18.285, 2.8477 ) [ Pull ( 22.731, 1.1482 ) ( 28.777, 1.6487 ), Pull ( 33.624, 2.1482 ) ( 36.871, 2.8477 ), Pull ( 39.919, 3.6969 ) ( 40.768, 4.9461 ) ]
        |> outlined (solid 0.5) (rgb 53 50 98)
        |> rotate (degrees -10)
    , curve ( 23.381, 4.0468 ) [ Pull ( 25.929, 2.847 ) ( 30.276, 3.4473 ), Pull ( 33.124, 3.9468 ) ( 34.772, 4.6463 ) ]
        |> outlined (solid 0.5) (rgb 53 50 98)
        |> rotate (degrees -10)
    ]
        |> group



-- cody fish


codyS model colour =
    group
        [ fishS (sandGradient 0 40 |> rotateGradient (degrees 180))
            |> move ( -20, 50 )
            |> move ( repeatDistance 60 500 100 -model.time, 0 )
        , rightFinS (sandGradient -10 100)
            |> move ( -20, 50 )
            |> move ( repeatDistance 60 500 100 -model.time, 0 )
            |> rotate (degrees 1.3 * sin (model.time * 10))
        , leftFinS (sandGradient -10 100)
            |> move ( -20, 50 )
            |> move ( repeatDistance 60 500 100 -model.time, 0 )
            |> rotate (degrees -1.3 * sin (model.time * 10))
        , fishTailS (sandGradient 10 100)
            |> move ( -20, 50 )
            |> move ( repeatDistance 60 500 100 -model.time, 0 )
            |> move ( 0, 2 * sin (model.time * 5) )
        , upperFinS (sandGradient 30 100 |> rotateGradient (degrees 180))
            |> move ( -20, 50 )
            |> move ( repeatDistance 60 500 100 -model.time, 0 )
        ]


fishTailS colour =
    group
        [ curve ( -7.044, -32.52 ) [ Pull ( -4.646, -30.1 ) ( -2.248, -29.52 ), Pull ( -3.896, -33.61 ) ( -5.545, -34.62 ), Pull ( -3.666, -35.85 ) ( -3.147, -39.11 ), Pull ( -5.096, -39.52 ) ( -7.044, -38.22 ), Pull ( -7.194, -36.42 ) ( -7.344, -34.62 ), Pull ( -7.644, -33.57 ) ( -7.044, -32.52 ) ]
            |> filled colour
        ]


leftFinS colour =
    group
        [ curve ( -32.82, -38.22 ) [ Pull ( -34.17, -38.81 ) ( -35.52, -39.41 ), Pull ( -35.07, -40.16 ) ( -34.62, -40.91 ), Pull ( -34.02, -40.61 ) ( -33.42, -40.31 ), Pull ( -33.27, -40.61 ) ( -33.12, -40.91 ), Pull ( -33.42, -41.36 ) ( -33.72, -41.81 ), Pull ( -32.93, -42.26 ) ( -31.62, -42.71 ), Pull ( -30.57, -43.16 ) ( -32.82, -38.22 ) ]
            |> filled colour
        ]


rightFinS colour =
    group
        [ curve ( -11.54, -36.72 ) [ Pull ( -13.18, -39.41 ) ( -14.83, -42.11 ), Pull ( -13.93, -42.26 ) ( -13.03, -42.41 ), Pull ( -12.74, -41.96 ) ( -12.44, -41.51 ), Pull ( -12.14, -41.96 ) ( -11.84, -42.41 ), Pull ( -10.94, -42.55 ) ( -10.04, -42.41 ), Pull ( -8.992, -41.96 ) ( -7.943, -41.51 ), Pull ( -8.683, -40.16 ) ( -11.54, -36.72 ) ]
            |> filled colour
        ]


upperFinS colour =
    group
        [ curve ( -26.82, -26.52 ) [ Pull ( -26.67, -24.28 ) ( -26.52, -22.03 ), Pull ( -24.88, -21.04 ) ( -23.23, -21.13 ), Pull ( -23.08, -22.03 ) ( -22.93, -22.93 ), Pull ( -22.48, -22.18 ) ( -22.03, -21.43 ), Pull ( -19.78, -20.47 ) ( -17.53, -20.83 ), Pull ( -17.53, -21.88 ) ( -17.53, -22.93 ), Pull ( -16.78, -22.18 ) ( -16.03, -21.43 ), Pull ( -13.48, -21.57 ) ( -10.94, -24.43 ), Pull ( -9.182, -27.3 ) ( -9.142, -29.82 ), Pull ( -11.54, -26.97 ) ( -13.93, -25.92 ), Pull ( -17.38, -24.37 ) ( -20.83, -24.73 ), Pull ( -23.83, -25.01 ) ( -26.82, -26.52 ) ]
            |> filled colour
        ]


fishS colour =
    group
        [ -- body
          oval 28 24
            |> filled colour
            |> move ( -20, -34 )

        -- right eye
        , eyeShellS
            |> move ( -30, -23 )

        -- left eye
        , eyeShellS
            |> move ( -41, -23 )
        , -- mouth
          mouthShellS
            |> scale 0.15
            |> scaleX -1
            |> move ( -22, -40 )
        ]



--------------------------------STARFISH--------------------------------------------------------


stopsStarfish t =
    List.range 0 10
        |> List.map
            (\idx ->
                stop (hsl (t + degrees 36 * toFloat idx) 1 0.5)
                    (5 * toFloat idx)
            )



{- myShapes model =
   [
   --rainbow model,
   movingStarfish model
   --|> scale (0.7+0.2*sin(1.5*model.time))
   --|> rotate (5*sin( 1*model.time))

   ]
-}
-- combines starfish and the animated rainbow gradient


movingStarfish model =
    group
        [ starfish
        , rainbow model
            |> clip pearls
        ]



-- animated rainbow gradient


rainbow model =
    group
        [ circle 50
            |> filled
                (radialGradient
                    (stopsStarfish (model.time * 0.4))
                    |> rotateGradient (degrees 90)
                )
            |> move ( -5, -0.5 )
        ]



-- original starfish outline


starfish =
    group
        [ curve ( 5.0659, 13.34 ) [ Pull ( 15.197, 14.522 ) ( 25.329, 15.704 ), Pull ( 29.223, 14.591 ) ( 27.356, 10.638 ), Pull ( 18.575, 4.7282 ) ( 9.7941, -1.182 ), Pull ( 8.9053, -2.679 ) ( 9.4564, -4.897 ), Pull ( 13.678, -13.5 ) ( 17.899, -22.12 ), Pull ( 18.846, -26.61 ) ( 12.833, -25.49 ), Pull ( 6.0791, -18.91 ) ( -0.675, -12.32 ), Pull ( -4.39, -9.104 ) ( -8.105, -13.0 ), Pull ( -13.5, -19.25 ) ( -18.91, -25.49 ), Pull ( -24.31, -26.55 ) ( -23.64, -21.44 ), Pull ( -20.77, -13.17 ) ( -17.89, -4.897 ), Pull ( -17.07, -3.039 ) ( -19.92, -1.182 ), Pull ( -28.36, 3.2084 ) ( -36.81, 7.5989 ), Pull ( -39.71, 11.78 ) ( -34.78, 13.002 ), Pull ( -24.99, 12.664 ) ( -15.19, 12.327 ), Pull ( -12.48, 11.549 ) ( -12.49, 14.691 ), Pull ( -9.794, 24.654 ) ( -7.092, 34.617 ), Pull ( -4.39, 37.786 ) ( -1.688, 34.955 ), Pull ( 0.1688, 24.992 ) ( 2.0263, 15.029 ), Pull ( 3.1661, 13.184 ) ( 5.0659, 13.34 ) ]
            |> filled (hsla (degrees 13) 0.84 0.5 1)
            |> addOutline (solid 6) (hsla (degrees 14) 0.95 0.3 1)
        , curve ( 0, 19.757 ) [ Pull ( -1.519, 11.482 ) ( -3.039, 3.2084 ), Pull ( -14.86, 3.715 ) ( -26.68, 4.2216 ), Pull ( -15.53, 0.8443 ) ( -4.39, -2.532 ), Pull ( -9.456, -14.01 ) ( -14.52, -25.49 ), Pull ( -6.585, -15.19 ) ( 1.3509, -4.897 ), Pull ( 9.2875, -14.69 ) ( 17.224, -24.48 ), Pull ( 12.158, -14.01 ) ( 7.0923, -3.546 ), Pull ( 17.562, 1.6886 ) ( 28.031, 6.9234 ), Pull ( 16.886, 4.897 ) ( 5.7414, 2.8707 ), Pull ( 2.7018, 11.313 ) ( 0, 19.757 ) ]
            |> filled (hsla (degrees 12.5) 0.5 0.47 1)
            |> move ( -5, 5 )
            |> scale 0.8
        ]



-- pearls on starfish


pearls =
    group
        [ circle 1
            |> filled white
        , circle 1
            |> filled white
            |> move ( 4, -7 )
        , circle 0.5
            |> filled white
            |> move ( 8, -13 )
        , circle 0.5
            |> filled white
            |> move ( -9, 0 )
        , circle 1
            |> filled white
            |> move ( -11.5, -7 )
        , circle 1
            |> filled white
            |> move ( -15, -13 )
        , circle 1
            |> filled white
            |> move ( 5, 6 )
        , circle 1
            |> filled white
            |> move ( 12, 8 )
        , circle 1
            |> filled white
            |> move ( 18, 10 )
        , circle 0.5
            |> filled white
            |> move ( -5, 12 )
        , circle 1
            |> filled white
            |> move ( -5, 18 )
        , circle 1
            |> filled white
            |> move ( -5, 23 )
        , circle 0.5
            |> filled white
            |> move ( -14, 5 )
        , circle 1
            |> filled white
            |> move ( -20.5, 6.5 )
        , circle 1
            |> filled white
            |> move ( -26, 8 )
        ]



------------------------------------------------------------------------------------------------------
--------------------------------CODY FISH----------------------------------------------------------


cody model colour =
    group
        [ -- fish 1
          fish yellow
            |> move ( 30, 40 )

        --|> move (repeatDistance 60 500 100 -model.time, 0)
        , upperFin orange
            |> move ( 30, 40 )

        --|> move (repeatDistance 60 500 100 -model.time, 0)
        , rightFin orange
            |> move ( 30, 40 )
            --|> move (repeatDistance 60 500 100 -model.time, 0)
            |> rotate (degrees 1.3 * sin (model.time * 10))
        , leftFin orange
            |> move ( 30, 40 )
            --|> move (repeatDistance 60 500 100 -model.time, 0)
            |> rotate (degrees -1.3 * sin (model.time * 10))
        , fishTail orange
            |> move ( 30, 40 )
            --|> move (repeatDistance 60 500 100 -model.time, 0)
            |> move ( 0, 2 * sin (model.time * 5) )
        ]


fishTail colour =
    group
        [ curve ( -7.044, -32.52 ) [ Pull ( -4.646, -30.1 ) ( -2.248, -29.52 ), Pull ( -3.896, -33.61 ) ( -5.545, -34.62 ), Pull ( -3.666, -35.85 ) ( -3.147, -39.11 ), Pull ( -5.096, -39.52 ) ( -7.044, -38.22 ), Pull ( -7.194, -36.42 ) ( -7.344, -34.62 ), Pull ( -7.644, -33.57 ) ( -7.044, -32.52 ) ]
            |> filled colour
        ]


leftFin colour =
    group
        [ curve ( -32.82, -38.22 ) [ Pull ( -34.17, -38.81 ) ( -35.52, -39.41 ), Pull ( -35.07, -40.16 ) ( -34.62, -40.91 ), Pull ( -34.02, -40.61 ) ( -33.42, -40.31 ), Pull ( -33.27, -40.61 ) ( -33.12, -40.91 ), Pull ( -33.42, -41.36 ) ( -33.72, -41.81 ), Pull ( -32.93, -42.26 ) ( -31.62, -42.71 ), Pull ( -30.57, -43.16 ) ( -32.82, -38.22 ) ]
            |> filled colour
        ]


rightFin colour =
    group
        [ curve ( -11.54, -36.72 ) [ Pull ( -13.18, -39.41 ) ( -14.83, -42.11 ), Pull ( -13.93, -42.26 ) ( -13.03, -42.41 ), Pull ( -12.74, -41.96 ) ( -12.44, -41.51 ), Pull ( -12.14, -41.96 ) ( -11.84, -42.41 ), Pull ( -10.94, -42.55 ) ( -10.04, -42.41 ), Pull ( -8.992, -41.96 ) ( -7.943, -41.51 ), Pull ( -8.683, -40.16 ) ( -11.54, -36.72 ) ]
            |> filled colour
        ]


upperFin colour =
    group
        [ curve ( -26.82, -26.52 ) [ Pull ( -26.67, -24.28 ) ( -26.52, -22.03 ), Pull ( -24.88, -21.04 ) ( -23.23, -21.13 ), Pull ( -23.08, -22.03 ) ( -22.93, -22.93 ), Pull ( -22.48, -22.18 ) ( -22.03, -21.43 ), Pull ( -19.78, -20.47 ) ( -17.53, -20.83 ), Pull ( -17.53, -21.88 ) ( -17.53, -22.93 ), Pull ( -16.78, -22.18 ) ( -16.03, -21.43 ), Pull ( -13.48, -21.57 ) ( -10.94, -24.43 ), Pull ( -9.182, -27.3 ) ( -9.142, -29.82 ), Pull ( -11.54, -26.97 ) ( -13.93, -25.92 ), Pull ( -17.38, -24.37 ) ( -20.83, -24.73 ), Pull ( -23.83, -25.01 ) ( -26.82, -26.52 ) ]
            |> filled colour
        ]


fish colour =
    group
        [ -- body
          oval 28 24
            |> filled colour
            |> move ( -20, -34 )

        -- right eye
        , circle 5
            |> filled white
            |> move ( -20, -33 )

        -- left eye
        , circle 5
            |> filled white
            |> move ( -31, -33 )

        -- right eye pupil
        , oval 5 7.5
            |> filled black
            |> move ( -22, -33 )

        -- left eye pupil
        , oval 5 7.5
            |> filled black
            |> move ( -33, -33 )

        -- mouth
        , curve ( -27.72, -38.22 ) [ Pull ( -26.97, -39.41 ) ( -26.22, -40.61 ), Pull ( -25.62, -40.61 ) ( -25.03, -40.61 ), Pull ( -24.13, -39.26 ) ( -23.23, -37.92 ), Pull ( -23.68, -37.77 ) ( -24.13, -37.62 ), Pull ( -24.58, -38.22 ) ( -25.03, -38.81 ), Pull ( -25.18, -39.26 ) ( -25.33, -39.71 ), Pull ( -25.62, -39.71 ) ( -25.92, -39.71 ), Pull ( -26.37, -38.81 ) ( -26.82, -37.92 ), Pull ( -27.27, -38.07 ) ( -27.72, -38.22 ) ]
            |> filled black
        ]



--------------------------------SEASHELL-----------------------------------------


angle =
    degrees 30


stops t =
    List.range 0 10
        |> List.map
            (\idx ->
                stop (hsl (3 + degrees 2 * toFloat idx) 1 0.5)
                    (5 * toFloat idx)
            )


myBlack =
    rgb 39 120 230



-- outline of the seashell


shell textLabel notifyMsg =
    -- the shell
    let
        shellStencil =
            curve ( -46.91, -5.845 ) [ Pull ( -50.34, -11.84 ) ( -46.01, -17.83 ), Pull ( -23.53, -33.17 ) ( -1.049, -35.52 ), Pull ( 20.084, -31.83 ) ( 41.217, -19.63 ), Pull ( 46.216, -11.98 ) ( 43.615, -6.145 ), Pull ( 47.835, 1.1091 ) ( 44.215, 8.2435 ), Pull ( 41.817, 11.572 ) ( 39.419, 10.941 ), Pull ( 40.35, 15.437 ) ( 37.32, 19.934 ), Pull ( 34.653, 23.612 ) ( 31.025, 25.33 ), Pull ( 28.098, 30.547 ) ( 23.531, 33.124 ), Pull ( 19.873, 33.964 ) ( 18.735, 31.925 ), Pull ( 12.449, 39.843 ) ( 7.0444, 34.922 ), Pull ( -0.978, 42.132 ) ( -10.04, 35.222 ), Pull ( -18.0, 37.983 ) ( -22.93, 31.625 ), Pull ( -29.47, 35.438 ) ( -34.62, 24.131 ), Pull ( -42.71, 24.086 ) ( -43.01, 10.042 ), Pull ( -52.16, 7.5983 ) ( -46.91, -5.845 ) ]
    in
    group
        [ shellStencil
            |> filled
                (gradient
                    (stops 0.4)
                    |> rotateGradient (degrees 90)
                 -- comment this line out for a vertical animation
                )

        -- shell bottom colour
        , curve ( -24.43, -30.72 ) [ Pull ( -24.88, -33.87 ) ( -25.33, -37.02 ), Pull ( -15.47, -38.59 ) ( -3.747, -36.72 ), Pull ( 8.843, -39.1 ) ( 21.433, -38.51 ), Pull ( 21.863, -34.17 ) ( 20.533, -29.82 ), Pull ( 8.5433, -32.82 ) ( -3.447, -35.82 ), Pull ( -13.33, -32.82 ) ( -23.23, -29.82 ), Pull ( -23.83, -30.42 ) ( -24.43, -30.72 ) ]
            |> filled
                -- remove "radial" in the following to get a linear gradient
                (radialGradient
                    (stops 0.4)
                    |> rotateGradient (degrees 90)
                )

        -- bottom
        , curve ( -24.43, -29.52 ) [ Pull ( -24.77, -33.74 ) ( -25.03, -37.32 ), Pull ( -18.27, -39.57 ) ( -3.447, -37.02 ), Pull ( 12.513, -39.07 ) ( 20.833, -38.51 ), Pull ( 21.823, -34.32 ) ( 20.533, -30.12 ), Pull ( 7.9325, -34.42 ) ( -1.348, -36.12 ), Pull ( -11.62, -35.4 ) ( -24.43, -29.52 ) ]
            |> outlined (solid 1) myBlack

        -- left line 1
        , curve ( -1.648, -35.52 ) [ Pull ( -17.98, -35.15 ) ( -40.91, -22.03 ), Pull ( -52.27, -13.93 ) ( -46.91, -5.845 ), Pull ( -40.54, -15.44 ) ( -31.62, -22.33 ) ]
            |> outlined (solid 1) myBlack

        -- left curves 2 and 3
        , curve ( -46.31, -5.845 ) [ Pull ( -52.85, 6.9381 ) ( -43.31, 10.641 ), Pull ( -40.32, -0.37 ) ( -34.62, -9.742 ), Pull ( -50.6, 19.424 ) ( -35.82, 24.43 ), Pull ( -34.92, 24.701 ) ( -34.02, 24.131 ), Pull ( -30.76, 2.9068 ) ( -21.43, -16.03 ) ]
            |> outlined (solid 1) myBlack

        --left curves 3 4 5
        , curve ( -34.62, 25.03 ) [ Pull ( -28.73, 35.697 ) ( -22.93, 31.925 ), Pull ( -22.83, 20.463 ) ( -21.13, 10.042 ), Pull ( -27.27, 42.932 ) ( -10.34, 35.222 ), Pull ( -9.652, 30.126 ) ( -10.04, 25.03 ), Pull ( -9.892, 29.976 ) ( -9.742, 34.922 ), Pull ( -1.498, 43.152 ) ( 6.7447, 35.222 ), Pull ( 11.757, -5.209 ) ( -1.348, -36.12 ), Pull ( 20.023, -16.69 ) ( 19.035, 27.728 ) ]
            |> outlined (solid 1) myBlack

        -- right curves
        , curve ( 7.0444, 34.922 ) [ Pull ( 13.479, 40.294 ) ( 19.035, 31.625 ), Pull ( 25.15, 36.147 ) ( 30.426, 25.03 ), Pull ( 30.169, 7.6934 ) ( 22.032, -8.243 ), Pull ( 29.989, 8.5433 ) ( 30.426, 25.33 ), Pull ( 39.752, 22.095 ) ( 39.119, 12.74 ), Pull ( 38.241, 5.6553 ) ( 34.922, -0.749 ), Pull ( 38.461, 5.2459 ) ( 38.519, 11.241 ), Pull ( 47.657, 9.9866 ) ( 44.515, -2.548 ), Pull ( 37.802, -13.99 ) ( 25.929, -23.53 ), Pull ( 35.833, -15.58 ) ( 42.416, -5.845 ), Pull ( 44.295, -5.433 ) ( 44.814, -9.742 ), Pull ( 44.466, -15.75 ) ( 39.718, -19.93 ), Pull ( 29.606, -26.29 ) ( 19.934, -30.12 ), Pull ( 9.8922, -33.47 ) ( -0.149, -35.22 ), Pull ( 18.197, -28.97 ) ( 31.625, -7.644 ) ]
            |> outlined (solid 1) myBlack

        -- bottom lines right
        , curve ( 18.735, -14.83 ) [ Pull ( 11.763, -30.46 ) ( -1.049, -35.52 ), Pull ( -11.22, -11.83 ) ( -10.64, 18.135 ) ]
            |> outlined (solid 1) myBlack

        -- middle line
        , curve ( -18.73, 2.548 ) [ Pull ( -15.6, -14.22 ) ( -7.644, -28.92 ) ]
            |> outlined (solid 1) myBlack

        -- left lines
        , curve ( -18.73, -19.33 ) [ Pull ( -13.31, -30.41 ) ( -2.847, -35.22 ), Pull ( -19.83, -32.04 ) ( -31.62, -14.53 ) ]
            |> outlined (solid 1) myBlack
        , text textLabel
            |> centered
            |> outlined (solid 2.3) white
            |> move ( 0, -5 )
            |> scale 1.5
        , text textLabel
            |> centered
            |> filled (rgb 45 8 255)
            |> move ( 0, -5 )
            |> scale 1.5
        , shellStencil |> ghost |> notifyTap notifyMsg
        ]



--------------------------------SAD MERMAID-----------------------------------


finalSadMermaid model =
    group
        [ completeSadMermaid model
            |> move ( 0, 10 * sin model.time * 0.5 )
        ]


completeSadMermaid model =
    group
        [ tailSad model
            |> scale 0.5
            |> move ( -68, -13 )
        , sadMermaid
            |> scale 0.75
            |> move ( -50, 15 )
        ]


tailSad model =
    group
        [ animationPieces
            (mkAnimationPieceVS addCurve
                subCurve
                scaleCurve
                flippers
            )
            (\t -> MyCurve ( 0, 0 ) [])
            (3 + 3 * cos model.time)
            |> drawCurve
            |> filled (rgb 244 96 180)
        ]


sadMermaid =
    group
        [ -- hair
          curve ( -47.81, 14.838 ) [ Pull ( -46.96, 12.488 ) ( -45.71, 14.538 ), Pull ( -51.21, 27.682 ) ( -45.11, 31.025 ), Pull ( -44.96, 33.223 ) ( -42.41, 35.822 ), Pull ( -35.72, 47.018 ) ( -19.63, 46.014 ), Pull ( 0.5573, 44.523 ) ( 0.7494, 21.433 ), Pull ( 1.9491, 19.983 ) ( 1.3489, 19.934 ), Pull ( 3.3487, 18.437 ) ( 1.9484, 13.339 ), Pull ( 2.6978, 11.639 ) ( 3.4473, 13.339 ), Pull ( 5.1985, 7.8915 ) ( 0.1498, 8.2435 ), Pull ( -0.8, 5.395 ) ( 1.6487, 4.3466 ), Pull ( 5.0978, 3.7456 ) ( 3.747, 6.7447 ), Pull ( 8.6459, 5.548 ) ( 6.7447, -1.648 ), Pull ( 4.1976, -6.996 ) ( -0.749, -6.744 ), Pull ( -0.147, -13.79 ) ( -5.545, -16.03 ), Pull ( -8.843, -18.03 ) ( -12.14, -17.23 ), Pull ( -20.68, -8.992 ) ( -29.22, -0.749 ), Pull ( -34.62, -2.45 ) ( -40.01, 1.0491 ), Pull ( -43.36, 4.6463 ) ( -43.91, 8.2435 ), Pull ( -47.81, 7.592 ) ( -49.91, 12.14 ), Pull ( -49.91, 14.439 ) ( -47.81, 14.838 ) ]
            |> filled (rgb 69 38 14)

        -- face
        , curve ( -37.62, 21.133 ) [ Pull ( -37.36, 17.335 ) ( -39.71, 15.138 ), Pull ( -37.27, 6.5915 ) ( -27.42, 6.4449 ), Pull ( -23.23, 5.6449 ) ( -19.03, 6.4449 ), Pull ( -9.789, 7.092 ) ( -7.344, 13.939 ), Pull ( -6.894, 14.388 ) ( -6.444, 14.838 ), Pull ( -3.895, 14.287 ) ( -4.346, 17.536 ), Pull ( -5.545, 19.236 ) ( -6.744, 17.536 ), Pull ( -8.392, 21.583 ) ( -12.44, 23.831 ), Pull ( -15.88, 25.679 ) ( -17.53, 29.526 ), Pull ( -22.13, 25.578 ) ( -30.12, 25.03 ), Pull ( -31.17, 21.081 ) ( -37.62, 21.133 ) ]
            |> filled (rgb 205 156 109)

        -- body
        , curve ( -19.33, 6.4449 ) [ Pull ( -19.63, 5.9953 ) ( -18.73, 5.5456 ), Pull ( -16.28, 5.1459 ) ( -15.43, 4.9461 ), Pull ( -14.23, 4.5484 ) ( -12.44, -1.049 ), Pull ( -12.24, -2.548 ) ( -11.84, -4.046 ), Pull ( -9.592, -8.243 ) ( -7.344, -12.44 ), Pull ( -6.744, -12.44 ) ( -6.145, -12.44 ), Pull ( -5.545, -11.99 ) ( -4.946, -11.54 ), Pull ( -4.046, -11.79 ) ( -4.346, -11.84 ), Pull ( -2.747, -11.99 ) ( -3.147, -12.74 ), Pull ( -2.097, -13.33 ) ( -2.847, -13.93 ), Pull ( -2.398, -14.08 ) ( -1.948, -14.23 ), Pull ( -2.047, -16.08 ) ( -4.346, -15.73 ), Pull ( -6.296, -15.63 ) ( -5.845, -18.13 ), Pull ( -7.344, -19.53 ) ( -7.644, -18.13 ), Pull ( -7.943, -17.68 ) ( -8.243, -17.23 ), Pull ( -9.092, -17.38 ) ( -9.742, -15.13 ), Pull ( -12.04, -13.98 ) ( -13.33, -10.64 ), Pull ( -14.23, -8.393 ) ( -15.13, -6.145 ), Pull ( -14.83, -9.142 ) ( -14.53, -12.14 ), Pull ( -22.93, -11.99 ) ( -31.32, -11.84 ), Pull ( -31.47, -9.892 ) ( -31.62, -7.943 ), Pull ( -31.32, -6.594 ) ( -31.02, -5.245 ), Pull ( -31.77, -6.295 ) ( -32.52, -7.344 ), Pull ( -33.62, -13.04 ) ( -36.72, -15.13 ), Pull ( -37.12, -16.38 ) ( -39.11, -17.83 ), Pull ( -40.01, -18.78 ) ( -40.91, -18.13 ), Pull ( -40.21, -17.23 ) ( -40.91, -16.33 ), Pull ( -41.91, -15.13 ) ( -44.51, -15.13 ), Pull ( -45.46, -14.63 ) ( -44.21, -13.93 ), Pull ( -44.56, -12.63 ) ( -43.31, -12.74 ), Pull ( -43.71, -12.29 ) ( -42.71, -11.84 ), Pull ( -42.46, -10.84 ) ( -41.21, -11.84 ), Pull ( -40.91, -11.99 ) ( -40.61, -12.14 ), Pull ( -40.16, -12.29 ) ( -39.71, -12.44 ), Pull ( -39.56, -12.29 ) ( -39.41, -12.14 ), Pull ( -36.57, -8.693 ) ( -34.92, -3.447 ), Pull ( -34.17, 0.5995 ) ( -32.22, 4.6463 ), Pull ( -31.22, 5.396 ) ( -28.62, 5.5456 ), Pull ( -27.12, 5.8454 ) ( -27.42, 6.1451 ), Pull ( -27.57, 6.4449 ) ( -27.72, 6.7447 ), Pull ( -23.83, 7.1943 ) ( -19.33, 6.4449 ) ]
            |> filled (rgb 205 156 109)

        -- waistband
        , curve ( -14.83, -7.344 ) [ Pull ( -15.68, -8.993 ) ( -18.13, -9.442 ), Pull ( -21.63, -9.842 ) ( -22.93, -10.64 ), Pull ( -24.93, -9.992 ) ( -27.72, -9.742 ), Pull ( -31.12, -9.443 ) ( -31.92, -7.344 ), Pull ( -34.37, -7.293 ) ( -34.02, -9.442 ), Pull ( -33.47, -10.64 ) ( -33.12, -10.64 ), Pull ( -33.32, -12.69 ) ( -30.72, -12.14 ), Pull ( -29.97, -13.99 ) ( -28.02, -13.03 ), Pull ( -26.02, -14.93 ) ( -23.23, -13.03 ), Pull ( -20.83, -14.83 ) ( -18.43, -13.03 ), Pull ( -17.48, -14.59 ) ( -15.73, -12.14 ), Pull ( -13.53, -13.19 ) ( -13.33, -10.04 ), Pull ( -11.98, -8.193 ) ( -14.83, -7.344 ) ]
            |> filled (rgb 250 203 9)

        -- chest shirt
        , curve ( -31.62, 1.0491 ) [ Pull ( -32.22, -2.098 ) ( -31.02, -5.245 ), Pull ( -23.93, -7.445 ) ( -15.43, -5.245 ), Pull ( -15.28, -2.398 ) ( -15.13, 0.4496 ), Pull ( -16.33, 2.7491 ) ( -17.53, 1.6487 ), Pull ( -19.03, 3.6484 ) ( -20.53, 2.2482 ), Pull ( -22.03, 3.5981 ) ( -23.53, 2.548 ), Pull ( -25.33, 3.6482 ) ( -27.12, 1.9484 ), Pull ( -28.32, 3.1985 ) ( -29.52, 1.6487 ), Pull ( -30.57, 2.0489 ) ( -31.62, 1.0491 ) ]
            |> filled (rgb 250 203 9)

        -- mouth
        , curve ( -25.92, 9.7423 ) [ Pull ( -23.93, 8.1423 ) ( -21.13, 9.7423 ) ]
            |> outlined (solid 1) red
            |> mirrorY
            |> move ( 0, 20 )

        -- star hair ornament
        , curve ( -14.23, 35.222 ) [ Pull ( -14.08, 33.873 ) ( -13.93, 32.524 ), Pull ( -14.53, 31.325 ) ( -15.13, 30.126 ), Pull ( -13.63, 29.826 ) ( -12.14, 29.526 ), Pull ( -11.09, 28.327 ) ( -10.04, 27.128 ), Pull ( -9.442, 28.477 ) ( -8.843, 29.826 ), Pull ( -7.494, 30.426 ) ( -6.145, 31.025 ), Pull ( -7.194, 32.074 ) ( -8.243, 33.124 ), Pull ( -8.543, 34.622 ) ( -8.843, 36.121 ), Pull ( -10.04, 35.522 ) ( -11.24, 34.922 ), Pull ( -12.74, 35.072 ) ( -14.23, 35.222 ) ]
            |> filled pink

        -- circle hair ornament (closest to star)
        , circle 1.5
            |> filled white
            |> move ( -7.5, 28 )

        -- circle hair ornament (2nd closest to star)
        , circle 1
            |> filled white
            |> move ( -5.5, 26 )

        -- circle hair ornament (3rd closest to star)
        , circle 0.75
            |> filled white
            |> move ( -5, 24 )

        -- circle hair ornament (4th closest to star)
        , circle 0.5
            |> filled white
            |> move ( -5, 22.5 )

        -- earring
        , circle 0.75
            |> filled pink
            |> move ( -7.25, 14.5 )

        -- right eye white
        , oval 4.5 3.8
            |> filled white
            |> move ( -14, 16.7 )

        -- left eye white
        , oval 4.5 3.8
            |> filled white
            |> move ( -33, 16.7 )

        -- right eye black
        , oval 2.4 3.3
            |> filled black
            |> move ( -14, 17.2 )
            |> clip
                (oval 4.5 3.8
                    |> filled white
                    |> move ( -14, 16.7 )
                )

        -- left eye black
        , oval 2.4 3.3
            |> filled black
            |> move ( -33, 17.2 )
            |> clip
                (oval 4.5 3.8
                    |> filled white
                    |> move ( -33, 16.7 )
                )
        ]



--------------------------------MERMAID-----------------------------------------


q0 =
    ( 40, 0 )


q1 =
    ( 0, 40 )


q2 =
    ( -40, 0 )


q3 =
    ( 0, -40 )


xCoord ( x, y ) =
    x


yCoord ( x, y ) =
    y


add ( x, y ) ( u, v ) =
    ( x + u, y + v )


sub ( x, y ) ( u, v ) =
    ( x - u, y - v )


scaleP s ( x, y ) =
    ( s * x, s * y )


finalMermaid model =
    group
        [ completeMermaid model
            |> move ( 0, 10 * sin model.time * 0.5 )
        ]


completeMermaid model =
    group
        [ tail model
            |> scale 0.5
            |> move ( -68, -13 )
        , mermaid
            |> scale 0.75
            |> move ( -50, 15 )
        ]


tail model =
    group
        [ animationPieces
            (mkAnimationPieceVS addCurve
                subCurve
                scaleCurve
                flippers
            )
            (\t -> MyCurve ( 0, 0 ) [])
            (3 + 3 * cos model.time)
            |> drawCurve
            |> filled (rgb 244 96 180)
        ]


mermaid =
    group
        [ -- hair
          curve ( -47.81, 14.838 ) [ Pull ( -46.96, 12.488 ) ( -45.71, 14.538 ), Pull ( -51.21, 27.682 ) ( -45.11, 31.025 ), Pull ( -44.96, 33.223 ) ( -42.41, 35.822 ), Pull ( -35.72, 47.018 ) ( -19.63, 46.014 ), Pull ( 0.5573, 44.523 ) ( 0.7494, 21.433 ), Pull ( 1.9491, 19.983 ) ( 1.3489, 19.934 ), Pull ( 3.3487, 18.437 ) ( 1.9484, 13.339 ), Pull ( 2.6978, 11.639 ) ( 3.4473, 13.339 ), Pull ( 5.1985, 7.8915 ) ( 0.1498, 8.2435 ), Pull ( -0.8, 5.395 ) ( 1.6487, 4.3466 ), Pull ( 5.0978, 3.7456 ) ( 3.747, 6.7447 ), Pull ( 8.6459, 5.548 ) ( 6.7447, -1.648 ), Pull ( 4.1976, -6.996 ) ( -0.749, -6.744 ), Pull ( -0.147, -13.79 ) ( -5.545, -16.03 ), Pull ( -8.843, -18.03 ) ( -12.14, -17.23 ), Pull ( -20.68, -8.992 ) ( -29.22, -0.749 ), Pull ( -34.62, -2.45 ) ( -40.01, 1.0491 ), Pull ( -43.36, 4.6463 ) ( -43.91, 8.2435 ), Pull ( -47.81, 7.592 ) ( -49.91, 12.14 ), Pull ( -49.91, 14.439 ) ( -47.81, 14.838 ) ]
            |> filled (rgb 69 38 14)

        -- face
        , curve ( -37.62, 21.133 ) [ Pull ( -37.36, 17.335 ) ( -39.71, 15.138 ), Pull ( -37.27, 6.5915 ) ( -27.42, 6.4449 ), Pull ( -23.23, 5.6449 ) ( -19.03, 6.4449 ), Pull ( -9.789, 7.092 ) ( -7.344, 13.939 ), Pull ( -6.894, 14.388 ) ( -6.444, 14.838 ), Pull ( -3.895, 14.287 ) ( -4.346, 17.536 ), Pull ( -5.545, 19.236 ) ( -6.744, 17.536 ), Pull ( -8.392, 21.583 ) ( -12.44, 23.831 ), Pull ( -15.88, 25.679 ) ( -17.53, 29.526 ), Pull ( -22.13, 25.578 ) ( -30.12, 25.03 ), Pull ( -31.17, 21.081 ) ( -37.62, 21.133 ) ]
            |> filled (rgb 205 156 109)

        -- body
        , curve ( -19.33, 6.4449 ) [ Pull ( -19.63, 5.9953 ) ( -18.73, 5.5456 ), Pull ( -16.28, 5.1459 ) ( -15.43, 4.9461 ), Pull ( -14.23, 4.5484 ) ( -12.44, -1.049 ), Pull ( -12.24, -2.548 ) ( -11.84, -4.046 ), Pull ( -9.592, -8.243 ) ( -7.344, -12.44 ), Pull ( -6.744, -12.44 ) ( -6.145, -12.44 ), Pull ( -5.545, -11.99 ) ( -4.946, -11.54 ), Pull ( -4.046, -11.79 ) ( -4.346, -11.84 ), Pull ( -2.747, -11.99 ) ( -3.147, -12.74 ), Pull ( -2.097, -13.33 ) ( -2.847, -13.93 ), Pull ( -2.398, -14.08 ) ( -1.948, -14.23 ), Pull ( -2.047, -16.08 ) ( -4.346, -15.73 ), Pull ( -6.296, -15.63 ) ( -5.845, -18.13 ), Pull ( -7.344, -19.53 ) ( -7.644, -18.13 ), Pull ( -7.943, -17.68 ) ( -8.243, -17.23 ), Pull ( -9.092, -17.38 ) ( -9.742, -15.13 ), Pull ( -12.04, -13.98 ) ( -13.33, -10.64 ), Pull ( -14.23, -8.393 ) ( -15.13, -6.145 ), Pull ( -14.83, -9.142 ) ( -14.53, -12.14 ), Pull ( -22.93, -11.99 ) ( -31.32, -11.84 ), Pull ( -31.47, -9.892 ) ( -31.62, -7.943 ), Pull ( -31.32, -6.594 ) ( -31.02, -5.245 ), Pull ( -31.77, -6.295 ) ( -32.52, -7.344 ), Pull ( -33.62, -13.04 ) ( -36.72, -15.13 ), Pull ( -37.12, -16.38 ) ( -39.11, -17.83 ), Pull ( -40.01, -18.78 ) ( -40.91, -18.13 ), Pull ( -40.21, -17.23 ) ( -40.91, -16.33 ), Pull ( -41.91, -15.13 ) ( -44.51, -15.13 ), Pull ( -45.46, -14.63 ) ( -44.21, -13.93 ), Pull ( -44.56, -12.63 ) ( -43.31, -12.74 ), Pull ( -43.71, -12.29 ) ( -42.71, -11.84 ), Pull ( -42.46, -10.84 ) ( -41.21, -11.84 ), Pull ( -40.91, -11.99 ) ( -40.61, -12.14 ), Pull ( -40.16, -12.29 ) ( -39.71, -12.44 ), Pull ( -39.56, -12.29 ) ( -39.41, -12.14 ), Pull ( -36.57, -8.693 ) ( -34.92, -3.447 ), Pull ( -34.17, 0.5995 ) ( -32.22, 4.6463 ), Pull ( -31.22, 5.396 ) ( -28.62, 5.5456 ), Pull ( -27.12, 5.8454 ) ( -27.42, 6.1451 ), Pull ( -27.57, 6.4449 ) ( -27.72, 6.7447 ), Pull ( -23.83, 7.1943 ) ( -19.33, 6.4449 ) ]
            |> filled (rgb 205 156 109)

        -- waistband
        , curve ( -14.83, -7.344 ) [ Pull ( -15.68, -8.993 ) ( -18.13, -9.442 ), Pull ( -21.63, -9.842 ) ( -22.93, -10.64 ), Pull ( -24.93, -9.992 ) ( -27.72, -9.742 ), Pull ( -31.12, -9.443 ) ( -31.92, -7.344 ), Pull ( -34.37, -7.293 ) ( -34.02, -9.442 ), Pull ( -33.47, -10.64 ) ( -33.12, -10.64 ), Pull ( -33.32, -12.69 ) ( -30.72, -12.14 ), Pull ( -29.97, -13.99 ) ( -28.02, -13.03 ), Pull ( -26.02, -14.93 ) ( -23.23, -13.03 ), Pull ( -20.83, -14.83 ) ( -18.43, -13.03 ), Pull ( -17.48, -14.59 ) ( -15.73, -12.14 ), Pull ( -13.53, -13.19 ) ( -13.33, -10.04 ), Pull ( -11.98, -8.193 ) ( -14.83, -7.344 ) ]
            |> filled (rgb 250 203 9)

        -- chest shirt
        , curve ( -31.62, 1.0491 ) [ Pull ( -32.22, -2.098 ) ( -31.02, -5.245 ), Pull ( -23.93, -7.445 ) ( -15.43, -5.245 ), Pull ( -15.28, -2.398 ) ( -15.13, 0.4496 ), Pull ( -16.33, 2.7491 ) ( -17.53, 1.6487 ), Pull ( -19.03, 3.6484 ) ( -20.53, 2.2482 ), Pull ( -22.03, 3.5981 ) ( -23.53, 2.548 ), Pull ( -25.33, 3.6482 ) ( -27.12, 1.9484 ), Pull ( -28.32, 3.1985 ) ( -29.52, 1.6487 ), Pull ( -30.57, 2.0489 ) ( -31.62, 1.0491 ) ]
            |> filled (rgb 250 203 9)

        -- mouth
        , curve ( -25.92, 9.7423 ) [ Pull ( -23.93, 8.1423 ) ( -21.13, 9.7423 ) ]
            |> outlined (solid 1) red

        -- star hair ornament
        , curve ( -14.23, 35.222 ) [ Pull ( -14.08, 33.873 ) ( -13.93, 32.524 ), Pull ( -14.53, 31.325 ) ( -15.13, 30.126 ), Pull ( -13.63, 29.826 ) ( -12.14, 29.526 ), Pull ( -11.09, 28.327 ) ( -10.04, 27.128 ), Pull ( -9.442, 28.477 ) ( -8.843, 29.826 ), Pull ( -7.494, 30.426 ) ( -6.145, 31.025 ), Pull ( -7.194, 32.074 ) ( -8.243, 33.124 ), Pull ( -8.543, 34.622 ) ( -8.843, 36.121 ), Pull ( -10.04, 35.522 ) ( -11.24, 34.922 ), Pull ( -12.74, 35.072 ) ( -14.23, 35.222 ) ]
            |> filled pink

        -- circle hair ornament (closest to star)
        , circle 1.5
            |> filled white
            |> move ( -7.5, 28 )

        -- circle hair ornament (2nd closest to star)
        , circle 1
            |> filled white
            |> move ( -5.5, 26 )

        -- circle hair ornament (3rd closest to star)
        , circle 0.75
            |> filled white
            |> move ( -5, 24 )

        -- circle hair ornament (4th closest to star)
        , circle 0.5
            |> filled white
            |> move ( -5, 22.5 )

        -- earring
        , circle 0.75
            |> filled pink
            |> move ( -7.25, 14.5 )

        -- right eye white
        , oval 4.5 3.8
            |> filled white
            |> move ( -14, 16.7 )

        -- left eye white
        , oval 4.5 3.8
            |> filled white
            |> move ( -33, 16.7 )

        -- right eye black
        , oval 2.4 3.3
            |> filled black
            |> move ( -14, 17.2 )
            |> clip
                (oval 4.5 3.8
                    |> filled white
                    |> move ( -14, 16.7 )
                )

        -- left eye black
        , oval 2.4 3.3
            |> filled black
            |> move ( -33, 17.2 )
            |> clip
                (oval 4.5 3.8
                    |> filled white
                    |> move ( -33, 16.7 )
                )
        ]



-- this is called bilinear interpolation
-- it follows the line between two points, tracing out a polygon


mkAnimationPiece listOfPoints =
    case listOfPoints of
        p0 :: p1 :: rest ->
            ( 1
            , \t ->
                add p0
                    (scaleP
                        t
                        (sub p1 p0)
                    )
            )
                :: mkAnimationPiece (p1 :: rest)

        _ ->
            []



{- mkAnimationPieceVS addVS subVS scaleVS listOfVectors =
   case listOfVectors of
     p0 :: p1 :: rest -> (1, \ t -> addVS p0
                               (scaleVS
                                  t
                                  (subVS p1 p0)
                               )
                         ) :: ( mkAnimationPieceVS addVS subVS scaleVS (p1 :: rest) )
     _ -> []
-}
--type MyCurve = MyCurve (Float,Float) (List Pull)
--drawCurve (MyCurve p0 pulls) = curve p0 pulls
--addPull (Pull p0 p1) (Pull p2 p3) = Pull (add p0 p2) (add p1 p3)
--subPull (Pull p0 p1) (Pull p2 p3) = Pull (sub p0 p2) (sub p1 p3)
--scalePull s (Pull p2 p3) = Pull (scaleP s p2) (scaleP s p3)
{- addCurve (MyCurve p0 pulls0) (MyCurve p1 pulls1) =
     MyCurve (add p0 p1) (List.map2 addPull pulls0 pulls1)
   subCurve (MyCurve p0 pulls0) (MyCurve p1 pulls1) =
     MyCurve (sub p0 p1) (List.map2 subPull pulls0 pulls1)
   scaleCurve s (MyCurve p1 pulls1) =
     MyCurve (scaleP s p1) (List.map (scalePull s) pulls1)
-}


flippers =
    [ MyCurve ( -9.442, 40.018 ) [ Pull ( -21.99, 9.1911 ) ( 1.6487, -17.23 ), Pull ( -8.408, -45.32 ) ( 21.133, -55.6 ), Pull ( 23.533, -42.26 ) ( 19.934, -27.12 ), Pull ( 34.173, -30.88 ) ( 48.412, -19.03 ), Pull ( 35.425, 2.1604 ) ( 13.039, -7.644 ), Pull ( 3.6903, 3.688 ) ( 11.54, 38.22 ), Pull ( -0.0, 43.619 ) ( -9.442, 40.018 ) ]
    , MyCurve ( -9.442, 40.018 ) [ Pull ( -23.39, 27.841 ) ( -5.545, -18.73 ), Pull ( -13.04, -43.01 ) ( 1.6487, -58.9 ), Pull ( 17.394, -47.61 ) ( 13.339, -29.52 ), Pull ( 23.927, -38.87 ) ( 44.515, -28.62 ), Pull ( 35.328, -8.883 ) ( 10.941, -13.33 ), Pull ( -1.358, -7.259 ) ( 10.941, 38.22 ), Pull ( 0.4496, 42.569 ) ( -9.442, 40.018 ) ]
    , MyCurve ( -9.742, 39.718 ) [ Pull ( -22.89, 22.091 ) ( -8.243, -17.53 ), Pull ( -25.68, -39.72 ) ( -17.53, -53.5 ), Pull ( -3.444, -57.11 ) ( 3.4473, -37.32 ), Pull ( 8.4892, -50.56 ) ( 24.131, -50.81 ), Pull ( 26.437, -31.02 ) ( 9.1428, -17.23 ), Pull ( 14.942, 11.091 ) ( 9.7423, 39.419 ), Pull ( -1.949, 41.919 ) ( -9.742, 39.718 ) ]
    , MyCurve ( -9.742, 39.419 ) [ Pull ( -23.79, 21.641 ) ( -9.442, -16.93 ), Pull ( -26.63, -21.32 ) ( -30.42, -40.31 ), Pull ( -19.33, -45.92 ) ( -8.243, -33.72 ), Pull ( -3.3, -46.31 ) ( 10.042, -48.71 ), Pull ( 15.144, -35.02 ) ( 5.2459, -19.33 ), Pull ( 16.493, 21.742 ) ( 10.341, 38.22 ), Pull ( -1.9, 43.669 ) ( -9.742, 39.419 ) ]
    , MyCurve ( -12.44, 38.519 ) [ Pull ( -14.74, 16.74 ) ( -11.24, -15.43 ), Pull ( -31.38, -23.57 ) ( -33.12, -37.92 ), Pull ( -21.13, -43.87 ) ( -10.94, -34.02 ), Pull ( -7.098, -47.66 ) ( 6.1451, -49.31 ), Pull ( 11.296, -35.17 ) ( 4.0468, -19.63 ), Pull ( 18.194, 15.992 ) ( 10.941, 38.819 ), Pull ( -0.599, 42.569 ) ( -12.44, 38.519 ) ]
    , MyCurve ( -10.34, 39.119 ) [ Pull ( -10.69, 13.089 ) ( -13.03, -13.33 ), Pull ( -33.08, -22.88 ) ( -35.52, -35.82 ), Pull ( -22.18, -41.82 ) ( -13.63, -32.82 ), Pull ( -10.49, -45.61 ) ( 1.0491, -50.21 ), Pull ( 10.599, -41.62 ) ( 0.7494, -18.43 ), Pull ( 17.545, 4.1423 ) ( 10.941, 37.92 ), Pull ( -0.299, 43.119 ) ( -10.34, 39.119 ) ]
    , MyCurve ( -11.84, 38.819 ) [ Pull ( -6.639, 22.239 ) ( -14.23, -12.14 ), Pull ( -31.12, -18.93 ) ( -38.22, -34.32 ), Pull ( -25.37, -40.12 ) ( -15.73, -32.52 ), Pull ( -14.14, -43.71 ) ( -1.948, -50.51 ), Pull ( 6.7512, -37.12 ) ( -1.348, -18.73 ), Pull ( 16.696, 0.6922 ) ( 10.341, 38.519 ), Pull ( -0.599, 42.119 ) ( -11.84, 38.819 ) ]
    , MyCurve ( 10.941, 38.519 ) [ Pull ( 19.744, 14.292 ) ( 2.548, -19.33 ), Pull ( 8.948, -37.67 ) ( 2.548, -52.6 ), Pull ( -12.89, -49.56 ) ( -14.53, -31.92 ), Pull ( -22.63, -40.67 ) ( -35.52, -34.62 ), Pull ( -38.33, -21.98 ) ( -13.33, -13.33 ), Pull ( -5.39, 1.5903 ) ( -11.84, 37.92 ), Pull ( -6.199, 42.97 ) ( 10.941, 38.519 ) ]
    , MyCurve ( 10.941, 38.819 ) [ Pull ( 12.996, -3.907 ) ( -2.548, -20.23 ), Pull ( 3.5039, -46.87 ) ( -7.644, -54.7 ), Pull ( -22.98, -49.01 ) ( -18.13, -26.52 ), Pull ( -32.17, -30.53 ) ( -43.61, -20.53 ), Pull ( -34.57, -6.788 ) ( -20.53, -10.04 ), Pull ( -6.487, -5.111 ) ( -12.44, 38.22 ), Pull ( -2.649, 42.42 ) ( 10.941, 38.819 ) ]
    ]


approachesOne t =
    (1 - 0.5 / (1 + t)) * 10


myRepeat t =
    3 * sin (0.3 * t)


liftOff t =
    if t < 2 then
        0

    else if t < 7 then
        10 * (t - 2)

    else
        50


liftOff2 t =
    animationPieces
        [ ( 2, \tt -> 0 )
        , ( 1, \tt -> 50 / 20 * (10 - 10 * cos (pi * tt)) )
        , ( 1, \tt -> 45 + 5 * cos (pi * tt) )
        , ( 2, \tt -> 40 )
        , ( 2, \tt -> 30 + 10 * cos (0.5 * pi * tt) )
        ]
        (\tt -> 20)
        t



-- needs at least 3 floats to interpolate
{- interpolateFloats floats t =
   let
     len = Array.length floats
     intTime = floor t
     fracTime = 0.5 * pi * (t - toFloat intTime)
     idx0 = modBy len intTime
     idx1 = modBy len (intTime + 1)
     idx2 = modBy len (intTime + 2)
     idx3 = modBy len(intTime + 3)
   in
     case (Array.get idx0 floats, Array.get idx1 floats, Array.get idx2 floats) of
       (Just p0, Just p1, Just p2) ->
         let
           p0top2 = p2 - p0
           midPoint = 0.5 * (p2 + p0)
           midTop1 = p1 - midPoint
         in
           sin fracTime
       _ ->
         0
-}
-------------------------------------------------------------------------------------
---------------------WATER BACKGROUND-----------------------------------


water brightness =
    group
        [ rect 500 500
            |> filled
                (gradient
                    [ --light blue
                      stop (rgb (brightness * 144) (brightness * 239) (brightness * 255)) -33

                    --blue
                    , stop (rgb (sqrt brightness * 20) (sqrt brightness * 187) (sqrt brightness * 220)) 30
                    ]
                    |> rotateGradient (degrees -90)
                )
        ]



-------------------------------------------------------------------
-------------------------------------------------------------------
-------------------------PUFFERFISH (CLOONIE)------------------------------------
-- pufferfish


pufferfish =
    group
        [ -- top spikes
          spike
            |> move ( 0, 13 )
        , spike
            |> rotate (degrees 40)
            |> move ( -11, 9 )
        , spike
            |> rotate (degrees -40)
            |> move ( 11, 9 )
        , -- bottom spikes
          spike
            |> scaleY -1
            |> move ( 0, -13 )
        , spike
            |> rotate (degrees 40)
            |> scaleY -1
            |> move ( -11, -9 )
        , spike
            |> rotate (degrees -40)
            |> scaleY -1
            |> move ( 11, -9 )
        , tailPuffer
            |> addOutline (solid 1) black
            |> rotate (degrees 90)
            |> move ( -20, 0 )
        , oval 30 25
            |> filled lightOrange
            |> addOutline (solid 0.5) black
        , -- spikes on the body
          spike
            |> rotate (degrees 20)
            |> move ( -5, 7 )
        , spike
            |> rotate (degrees -20)
            |> move ( 5, 7 )
        , spike
            |> rotate (degrees 20)
            |> scaleY -1
            |> move ( -5, -7 )
        , spike
            |> rotate (degrees -20)
            |> scaleY -1
            |> move ( 5, -7 )
        , -- eyes
          oval 3 4
            |> filled black
            |> move ( 0, 2 )
        , oval 3 4
            |> filled black
            |> move ( 9, 2 )
        , -- smile
          wedge 2 0.5
            |> filled black
            |> rotate (degrees -90)
            |> move ( 4.5, -2 )
        , -- blush
          oval 3 1
            |> filled pink
            |> move ( 0, -1 )
        , oval 3 1
            |> filled pink
            |> move ( 9, -1 )
        , -- fins
          wedge 5 0.25
            |> filled lightOrange
            |> addOutline (solid 0.5) black
            |> rotate (degrees -180)
            |> subtract (rect 4 10 |> filled white |> move ( 1, 0 ))
            |> move ( -7, 0 )
        , wedge 5 0.25
            |> filled lightOrange
            |> addOutline (solid 0.5) black
            |> subtract (rect 4 10 |> filled white)
            |> move ( 12.5, 0 )
        , text "YOU! (Cloonie)"
            |> bold
            |> filled lightBlue
            |> mirrorX
            |> scale 0.5
            |> move ( 19.25, -20 )
        , text "YOU! (Cloonie)"
            |> bold
            |> filled black
            |> mirrorX
            |> scale 0.5
            |> move ( 19, -20 )
        ]


spike =
    triangle 3
        |> filled lightOrange
        |> addOutline (solid 0.5) black
        |> rotate (degrees -30)
        |> subtract (rect 7 2.5 |> filled white |> move ( 0, -2 ))


tailPuffer =
    group
        [ circle 3.5
            |> filled lightOrange
            |> move ( -3, 0 )
        , circle 3.5
            |> filled lightOrange
            |> move ( 3, 0 )
        , triangle 7
            |> filled lightOrange
            |> rotate (degrees 30)
            |> move ( 0, -5 )
        ]



-----------------------------------------------------


type alias Model =
    { time : Float
    , state : State
    , startTime : Float
    }


type alias Point =
    ( Float, Float )


init : Model
init =
    { time = 0
    , state = Welcome True
    , startTime = 0
    }


view model =
    collage 192 128 (myShapes model)


main =
    gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }
