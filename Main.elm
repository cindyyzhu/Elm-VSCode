module Main exposing (main, myShapes, pizza)

import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)


type Msg
    = Tick Float GetKeyState


main =
    gameApp Tick
        { model = ()
        , view = \m -> collage 500 500 (myShapes m)
        , update = \_ m -> m
        , title = "Pizza"
        }


myShapes model =
    [ pizza
        |> scale 0.5
    ]


pizza =
    group
        [ -- Crust (outermost ring)
          circle 110
            |> filled (rgb 210 150 80)
        , -- Sauce layer
          circle 100
            |> filled (rgb 200 60 40)
        , -- Cheese layer
          circle 90
            |> filled (rgb 245 210 100)
        , -- Pepperoni 1 (top center)
          circle 14
            |> filled (rgb 180 40 30)
            |> move ( 0, 55 )
        , -- Pepperoni highlight 1
          circle 5
            |> filled (rgb 220 80 60)
            |> move ( -4, 59 )
        , -- Pepperoni 2 (left)
          circle 14
            |> filled (rgb 180 40 30)
            |> move ( -48, 20 )
        , -- Pepperoni highlight 2
          circle 5
            |> filled (rgb 220 80 60)
            |> move ( -52, 24 )
        , -- Pepperoni 3 (right)
          circle 14
            |> filled (rgb 180 40 30)
            |> move ( 48, 20 )
        , -- Pepperoni highlight 3
          circle 5
            |> filled (rgb 220 80 60)
            |> move ( 44, 24 )
        , -- Pepperoni 4 (bottom left)
          circle 14
            |> filled (rgb 180 40 30)
            |> move ( -30, -50 )
        , -- Pepperoni highlight 4
          circle 5
            |> filled (rgb 220 80 60)
            |> move ( -34, -46 )
        , -- Pepperoni 5 (bottom right)
          circle 14
            |> filled (rgb 180 40 30)
            |> move ( 30, -50 )
        , -- Pepperoni highlight 5
          circle 5
            |> filled (rgb 220 80 60)
            |> move ( 26, -46 )
        , -- Pepperoni 6 (center)
          circle 14
            |> filled (rgb 180 40 30)
            |> move ( 0, 0 )
        , -- Pepperoni highlight 6
          circle 5
            |> filled (rgb 220 80 60)
            |> move ( -4, 4 )
        , -- Slice lines (cut marks)
          rect 2 200
            |> filled (rgb 160 100 50)
            |> makeTransparent 0.5
        , rect 2 200
            |> filled (rgb 160 100 50)
            |> makeTransparent 0.5
            |> rotate (degrees 60)
        , rect 2 200
            |> filled (rgb 160 100 50)
            |> makeTransparent 0.5
            |> rotate (degrees 120)
        ]
