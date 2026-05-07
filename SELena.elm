-- Helpers at top level so both myShapes and update can use them


module Main exposing (Model, Msg(..), Phase(..), Spot, bucket, init, isCleaned, main, myShapes, nearSpot, sparkles2, sparkles3, spotPositions, update, view, washSpotPositions)

--import Geometry.Curve exposing (Pull(..))
--import Geometry.Svg exposing (curve)

--import Curve exposing (Pull(..))
import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)


nearSpot tx ty sx sy r =
    (tx - sx) ^ 2 + (ty - sy) ^ 2 < r ^ 2


isCleaned id cleaned =
    List.member id cleaned



-- (id, x, y) in collage coordinates matching allSpotData below


spotPositions =
    [ ( 1, 5, -10 )
    , ( 2, 18, 5 )
    , ( 3, 25, -20 )
    , ( 4, 0, 10 )
    , ( 5, 12, -5 )
    , ( 6, 28, 8 )
    ]


washSpotPositions =
    [ ( 7, 6, -10 )
    , ( 8, 0, 5 )
    , ( 9, -10, -20 )
    , ( 10, 0, 10 )
    , ( 11, -10, -5 )
    , ( 12, 0, 8 )
    ]


myShapes model =
    let
        phase =
            model.phase

        stableBg =
            [ -- Back wall (dark wood planks)
              rect 192 128 |> filled (rgb 80 50 25)

            -- Horizontal wood plank lines on back wall
            , rect 192 2 |> filled (rgb 60 38 18) |> move ( 0, 30 )
            , rect 192 2 |> filled (rgb 60 38 18) |> move ( 0, 10 )
            , rect 192 2 |> filled (rgb 60 38 18) |> move ( 0, -10 )
            , rect 192 2 |> filled (rgb 60 38 18) |> move ( 0, -30 )
            , rect 192 2 |> filled (rgb 60 38 18) |> move ( 0, -50 )

            -- Floor (straw-covered dirt)
            , rect 192 40 |> filled (rgb 139 101 40) |> move ( 0, -54 )

            {- -- Straw patches on floor
               , oval 30 6 |> filled (rgb 210 170 60) |> move (-40, -50)
               , oval 20 5 |> filled (rgb 200 160 55) |> move (20, -56)
               , oval 25 5 |> filled (rgb 215 175 65) |> move (60, -52)
               , oval 18 4 |> filled (rgb 205 165 58) |> move (-70, -58)
               , oval 28 6 |> filled (rgb 210 170 60) |> move (-10, -60)
            -}
            -- Left stall wall
            , rect 30 128 |> filled (rgb 101 65 30) |> move ( -81, 0 )

            -- Right stall wall
            , rect 30 128 |> filled (rgb 101 65 30) |> move ( 81, 0 )

            -- Left wall vertical plank lines
            , rect 2 128 |> filled (rgb 80 50 20) |> move ( -90, 0 )
            , rect 2 128 |> filled (rgb 80 50 20) |> move ( -75, 0 )

            -- Right wall vertical plank lines
            , rect 2 128 |> filled (rgb 80 50 20) |> move ( 75, 0 )
            , rect 2 128 |> filled (rgb 80 50 20) |> move ( 90, 0 )

            -- Stall door/gate (lower half bars - left side)
            , rect 4 50 |> filled (rgb 120 78 35) |> move ( -66, -34 )
            , rect 4 50 |> filled (rgb 120 78 35) |> move ( -50, -34 )
            , rect 4 50 |> filled (rgb 120 78 35) |> move ( -34, -34 )

            -- Stall gate horizontal rail (left)
            , rect 55 5 |> filled (rgb 90 55 22) |> move ( -49, -12 )
            , rect 55 5 |> filled (rgb 90 55 22) |> move ( -49, -35 )
            , rect 55 5 |> filled (rgb 90 55 22) |> move ( -49, -58 )

            -- Stall door/gate (lower half bars - right side)
            , rect 4 50 |> filled (rgb 120 78 35) |> move ( 34, -34 )
            , rect 4 50 |> filled (rgb 120 78 35) |> move ( 50, -34 )
            , rect 4 50 |> filled (rgb 120 78 35) |> move ( 66, -34 )

            -- Stall gate horizontal rail (right)
            , rect 55 5 |> filled (rgb 90 55 22) |> move ( 49, -12 )
            , rect 55 5 |> filled (rgb 90 55 22) |> move ( 49, -35 )
            , rect 55 5 |> filled (rgb 90 55 22) |> move ( 49, -58 )

            ----------HOOK TO HANG THE BUCKET!!!!----
            -- Wall mounting plate
            , roundedRect 24 22 1 |> filled (rgb 74 74 74) |> move ( 0, 22 ) |> scale 0.2 |> move ( 34, -14 )
            , roundedRect 20 18 1 |> filled (rgb 58 58 58) |> move ( 0, 22 ) |> scale 0.2 |> move ( 34, -14 )

            -- Plate rivets
            , circle 3 |> filled (rgb 85 85 85) |> move ( -5, 26 ) |> scale 0.2 |> move ( 34, -14 )
            , circle 3 |> filled (rgb 85 85 85) |> move ( 5, 26 ) |> scale 0.2 |> move ( 34, -14 )
            , circle 3 |> filled (rgb 85 85 85) |> move ( -5, 18 ) |> scale 0.2 |> move ( 34, -14 )
            , circle 3 |> filled (rgb 85 85 85) |> move ( 5, 18 ) |> scale 0.2 |> move ( 34, -14 )

            -- Hook vertical shaft
            , roundedRect 14 50 1 |> filled (rgb 74 74 74) |> move ( 0, -6 ) |> scale 0.2 |> move ( 34, -14 )
            , roundedRect 4 50 1 |> filled (rgb 90 90 90) |> move ( -2, -6 ) |> scale 0.2 |> move ( 34, -14 )

            -- Hook curve (approximate with rects and circles)
            , circle 20 |> outlined (solid 14) (rgb 74 74 74) |> move ( 20, -30 ) |> scale 0.2 |> move ( 34, -14 )
            , circle 20 |> outlined (solid 6) (rgb 90 90 90) |> move ( 20, -30 ) |> makeTransparent 0.5 |> scale 0.2 |> move ( 34, -14 )

            -- Cover top half of circle to keep only lower curve
            , rect 60 28 |> filled (rgb 80 50 25) |> move ( 37, -15 ) |> scale 0.2 |> move ( 34, -14 ) -- mask (use bg color)

            -- Hook tip
            , circle 8 |> filled (rgb 74 74 74) |> move ( 40, -30 ) |> scale 0.2 |> move ( 34, -14 )
            , circle 4 |> filled (rgb 58 58 58) |> move ( 41, -30 ) |> scale 0.2 |> move ( 34, -14 )

            {--- Hay rack on back wall
      , rect 50 4 |> filled (rgb 60 40 15) |> move (0, 20)
      , rect 50 4 |> filled (rgb 60 40 15) |> move (0, 8)
      , rect 4 16 |> filled (rgb 60 40 15) |> move (-23, 14)
      , rect 4 16 |> filled (rgb 60 40 15) |> move (-15, 14)
      , rect 4 16 |> filled (rgb 60 40 15) |> move (-7, 14)
      , rect 4 16 |> filled (rgb 60 40 15) |> move (1, 14)
      , rect 4 16 |> filled (rgb 60 40 15) |> move (9, 14)
      , rect 4 16 |> filled (rgb 60 40 15) |> move (17, 14)
      , rect 4 16 |> filled (rgb 60 40 15) |> move (25, 14)

      -- Hay in rack
      , oval 18 6 |> filled (rgb 220 180 60) |> move (-8, 18)
      , oval 14 5 |> filled (rgb 210 170 55) |> move (10, 17)
      , oval 10 4 |> filled (rgb 225 185 65) |> move (-18, 16)-}
            {- -- bucket handle
               , circle 7 |> outlined (solid 2) (rgb 130 100 60) |> move (-55, -38)

               -- Water bucket
               , rect 14 12 |> filled (rgb 50 80 130) |> move (-55, -47)
               , rect 16 3 |> filled (rgb 40 65 110) |> move (-55, -41)
            -}
            -- Ambient shadow at base of walls
            , rect 192 8 |> filled (rgba 0 0 0 0.3) |> move ( 0, -60 )
            , rect 10 128 |> filled (rgba 0 0 0 0.25) |> move ( -76, 0 )
            , rect 10 128 |> filled (rgba 0 0 0 0.25) |> move ( 76, 0 )

            -- Ceiling beam
            , rect 192 10 |> filled (rgb 70 45 20) |> move ( 0, 59 )
            , rect 192 3 |> filled (rgb 50 32 14) |> move ( 0, 53 )
            ]

        {- hay =
           [ rect 35 8 |> filled (rgb 220 190 80) |> move (-65, -58)
           , rect 25 5 |> filled (rgb 210 180 70) |> move (-60, -54)
           , rect 30 5 |> filled (rgb 230 200 90) |> move (-68, -53)
           ]
        -}
        windowPane =
            [ rect 28 22 |> filled (rgb 180 220 255) |> move ( 0, 35 ) |> scale 2 |> move ( 0, -50 )
            , rect 28 2 |> filled (rgb 80 45 10) |> move ( 0, 35 ) |> scale 2 |> move ( 0, -50 )
            , rect 2 22 |> filled (rgb 80 45 10) |> move ( 0, 35 ) |> scale 2 |> move ( 0, -50 )
            , rect 32 3 |> filled (rgb 80 45 10) |> move ( 0, 47 ) |> scale 2 |> move ( 0, -50 )
            , rect 32 3 |> filled (rgb 80 45 10) |> move ( 0, 24 ) |> scale 2 |> move ( 0, -50 )
            ]

        stars =
            [{- circle 1 |> filled white |> move (5 + 2 * sin model.time, 40 + sin (model.time * 1.3))
                , circle 1 |> filled white |> move (-5 + sin (model.time * 0.8), 38 + cos (model.time * 1.1))
                , circle 1 |> filled white |> move (10 + sin (model.time * 1.5), 43 + sin model.time)
                , circle 0.7 |> filled white |> move (-8, 42 + sin (model.time * 0.9))
             -}
            ]

        unicornGroup =
            group
                [ curve ( -53.35, -4.046 ) [ Pull ( -51.55, -2.737 ) ( -49.76, -1.948 ), Pull ( -51.7, 0.3823 ) ( -53.35, -4.046 ) ]
                    |> filled (rgb 30 131 189)
                , curve ( -40.16, -10.64 ) [ Pull ( -42.71, -6.295 ) ( -45.26, -1.948 ), Pull ( -48.11, -0.599 ) ( -50.96, 0.7494 ), Pull ( -56.5, -3.147 ) ( -62.05, -7.044 ), Pull ( -60.25, -11.39 ) ( -58.45, -15.73 ), Pull ( -63.25, -27.12 ) ( -68.04, -38.51 ), Pull ( -63.27, -41.63 ) ( -58.45, -38.51 ), Pull ( -51.51, -35.11 ) ( -51.25, -27.42 ), Pull ( -50.46, -20.23 ) ( -53.35, -13.03 ), Pull ( -53.61, -7.033 ) ( -52.15, -2.548 ), Pull ( -51.8, -2.048 ) ( -49.76, -1.948 ), Pull ( -46.46, -5.096 ) ( -43.16, -8.243 ), Pull ( -41.81, -9.292 ) ( -40.16, -10.64 ) ]
                    |> filled (rgb 30 131 189)
                , curve ( -42.86, -5.545 ) [ Pull ( -43.58, -0.408 ) ( -48.86, 2.8477 ), Pull ( -55.0, 3.8969 ) ( -61.15, 4.9461 ), Pull ( -65.22, -4.124 ) ( -63.25, -18.43 ), Pull ( -63.85, -21.88 ) ( -64.44, -25.33 ), Pull ( -65.01, -28.39 ) ( -68.34, -31.02 ), Pull ( -73.29, -34.26 ) ( -78.23, -30.42 ), Pull ( -76.6, -36.77 ) ( -68.04, -38.51 ), Pull ( -56.29, -36.07 ) ( -55.45, -23.23 ), Pull ( -57.35, -13.07 ) ( -55.45, -5.245 ), Pull ( -51.69, 0.303 ) ( -46.16, -2.548 ), Pull ( -44.66, -6.594 ) ( -43.16, -10.64 ), Pull ( -43.01, -7.793 ) ( -42.86, -5.545 ) ]
                    |> filled (rgb 30 189 125)
                , curve ( -43.76, -4.646 ) [ Pull ( -44.0, -0.599 ) ( -46.16, 3.4473 ), Pull ( -51.55, 5.6955 ) ( -56.95, 7.9437 ), Pull ( -62.35, 4.9461 ) ( -67.74, 1.9484 ), Pull ( -68.94, -4.196 ) ( -70.14, -10.34 ), Pull ( -68.94, -14.38 ) ( -67.74, -18.43 ), Pull ( -66.54, -20.53 ) ( -65.34, -22.63 ), Pull ( -66.99, -24.43 ) ( -68.64, -26.22 ), Pull ( -66.39, -25.77 ) ( -64.14, -25.33 ), Pull ( -61.6, -21.97 ) ( -62.05, -16.93 ), Pull ( -63.58, -9.621 ) ( -62.95, -4.346 ), Pull ( -61.67, 0.6007 ) ( -57.55, 2.548 ), Pull ( -47.05, 5.5903 ) ( -43.76, -4.646 ) ]
                    |> filled (rgb 237 235 81)
                , curve ( -41.36, -3.147 ) [ Pull ( -41.96, -1.648 ) ( -42.56, -0.149 ), Pull ( -44.83, 8.3157 ) ( -54.25, 10.941 ), Pull ( -62.19, 12.632 ) ( -68.64, 7.644 ), Pull ( -76.58, -0.048 ) ( -72.24, -12.14 ), Pull ( -67.92, -16.76 ) ( -69.24, -22.63 ), Pull ( -73.33, -28.19 ) ( -76.74, -19.63 ), Pull ( -79.05, -25.46 ) ( -74.04, -29.52 ), Pull ( -70.0, -32.86 ) ( -66.24, -28.92 ), Pull ( -61.64, -23.11 ) ( -65.04, -14.53 ), Pull ( -69.32, 1.8536 ) ( -59.95, 5.2459 ), Pull ( -50.92, 9.2583 ) ( -44.06, -1.049 ), Pull ( -42.71, -4.196 ) ( -41.36, -7.344 ), Pull ( -41.36, -5.245 ) ( -41.36, -3.147 ) ]
                    |> filled (rgb 237 81 180)
                , curve ( 0.8992, -21.73 ) [ Pull ( 1.4781, -26.65 ) ( 3.8969, -30.42 ), Pull ( 6.2273, -37.17 ) ( 2.9976, -43.91 ), Pull ( 1.559, -46.31 ) ( -0.599, -48.71 ), Pull ( -2.248, -49.91 ) ( -3.896, -51.11 ), Pull ( -4.196, -47.21 ) ( -4.496, -43.31 ), Pull ( -1.757, -39.56 ) ( -2.697, -35.82 ), Pull ( -3.476, -32.67 ) ( -6.295, -29.52 ), Pull ( -2.847, -25.62 ) ( 0.5995, -21.73 ), Pull ( 0.5995, -21.73 ) ( 0.8992, -21.73 ) ]
                    |> filled white
                    |> addOutline (solid 1) black
                , curve ( -8.393, -28.62 ) [ Pull ( -11.94, -40.01 ) ( -8.093, -51.4 ), Pull ( -8.543, -53.65 ) ( -8.992, -55.9 ), Pull ( -12.29, -53.95 ) ( -15.58, -52.0 ), Pull ( -18.48, -42.41 ) ( -19.18, -32.82 ), Pull ( -15.43, -29.52 ) ( -11.69, -26.22 ), Pull ( -10.19, -27.57 ) ( -8.393, -28.62 ) ]
                    |> filled white
                    |> addOutline (solid 1) black
                , curve ( -27.87, -31.32 ) [ Pull ( -28.26, -33.72 ) ( -29.97, -36.12 ), Pull ( -32.02, -40.92 ) ( -29.07, -47.21 ), Pull ( -29.67, -49.16 ) ( -30.27, -51.11 ), Pull ( -33.42, -48.86 ) ( -36.57, -46.61 ), Pull ( -38.23, -41.36 ) ( -38.96, -36.12 ), Pull ( -34.97, -33.23 ) ( -34.77, -29.82 ), Pull ( -29.67, -26.97 ) ( -24.58, -24.13 ), Pull ( -26.37, -27.87 ) ( -28.17, -31.62 ) ]
                    |> filled white
                    |> addOutline (solid 1) black
                , curve ( -15.88, 33.423 ) [ Pull ( -25.07, 39.056 ) ( -22.78, 52.009 ), Pull ( -12.68, 48.844 ) ( -10.79, 39.718 ), Pull ( -5.545, 44.515 ) ( -0.299, 49.311 ), Pull ( 2.0983, 47.812 ) ( 4.4964, 46.313 ), Pull ( 7.1943, 48.861 ) ( 9.8922, 51.409 ), Pull ( 12.74, 52.989 ) ( 15.587, 53.208 ), Pull ( 16.408, 48.562 ) ( 14.388, 43.915 ), Pull ( 14.538, 40.018 ) ( 14.688, 36.121 ), Pull ( 16.838, 30.276 ) ( 14.988, 24.43 ), Pull ( 18.977, 18.585 ) ( 16.487, 12.74 ), Pull ( 10.513, 6.5216 ) ( 0.2997, 8.5433 ), Pull ( 6.2995, -11.48 ) ( 0.8992, -22.63 ), Pull ( -2.937, -27.11 ) ( -8.093, -29.52 ), Pull ( -18.89, -34.31 ) ( -31.77, -30.42 ), Pull ( -33.41, -29.8 ) ( -34.17, -28.62 ), Pull ( -36.18, -33.57 ) ( -42.26, -38.51 ), Pull ( -44.64, -42.95 ) ( -41.66, -51.11 ), Pull ( -41.21, -53.35 ) ( -40.76, -55.6 ), Pull ( -42.71, -55.6 ) ( -44.66, -55.6 ), Pull ( -46.61, -53.65 ) ( -48.56, -51.7 ), Pull ( -50.73, -44.21 ) ( -51.25, -36.72 ), Pull ( -47.91, -30.19 ) ( -49.16, -23.23 ), Pull ( -52.24, -12.89 ) ( -49.76, -8.843 ), Pull ( -43.89, 3.1037 ) ( -26.37, -0.749 ), Pull ( -21.88, 8.2435 ) ( -17.38, 17.236 ), Pull ( -19.53, 26.66 ) ( -15.88, 33.423 ) ]
                    |> filled white
                    |> addOutline (solid 1) black
                , curve ( 0, 8.5433 ) [ Pull ( -7.614, 9.6405 ) ( -13.78, 15.737 ) ]
                    |> outlined (solid 1) black
                , curve ( -6.894, -22.63 ) [ Pull ( -7.644, -25.92 ) ( -8.393, -29.22 ) ]
                    |> outlined (solid 1) black
                , curve ( -4.496, -42.71 ) [ Pull ( -5.915, -44.02 ) ( -6.295, -46.01 ), Pull ( -6.386, -50.66 ) ( -3.597, -52.3 ), Pull ( -1.378, -51.24 ) ( -1.199, -49.01 ), Pull ( -3.467, -46.26 ) ( -4.496, -42.71 ) ]
                    |> filled (rgb 35 173 204)
                    |> addOutline (solid 1) black
                , curve ( -8.093, -52.9 ) [ Pull ( -6.184, -54.85 ) ( -6.594, -56.8 ), Pull ( -10.34, -58.8 ) ( -14.08, -56.8 ), Pull ( -15.01, -55.63 ) ( -15.58, -52.9 ), Pull ( -11.99, -54.24 ) ( -8.093, -52.9 ) ]
                    |> filled (rgb 35 173 204)
                    |> addOutline (solid 1) black
                , curve ( -28.47, -47.51 ) [ Pull ( -27.07, -49.46 ) ( -26.67, -51.4 ), Pull ( -30.87, -52.97 ) ( -35.07, -51.11 ), Pull ( -35.99, -49.31 ) ( -36.27, -47.51 ), Pull ( -32.37, -48.94 ) ( -28.47, -47.51 ) ]
                    |> filled (rgb 35 173 204)
                    |> addOutline (solid 1) black
                , curve ( -41.66, -52.0 ) [ Pull ( -39.65, -53.8 ) ( -40.16, -55.6 ), Pull ( -44.06, -57.49 ) ( -47.96, -55.3 ), Pull ( -48.83, -53.65 ) ( -48.86, -52.0 ), Pull ( -45.41, -53.68 ) ( -41.66, -52.0 ) ]
                    |> filled (rgb 35 173 204)
                    |> addOutline (solid 1) black
                , curve ( -19.78, 47.512 ) [ Pull ( -20.45, 36.807 ) ( -12.88, 35.822 ), Pull ( -10.78, 44.007 ) ( -19.78, 47.512 ) ]
                    |> filled (rgb 222 93 170)
                , curve ( 5.6955, 15.437 ) [ Pull ( 5.7049, 13.948 ) ( 7.1943, 13.339 ), Pull ( 7.9649, 14.568 ) ( 5.6955, 15.437 ) ]
                    |> filled black
                , curve ( 14.388, 16.936 ) [ Pull ( 13.568, 16.097 ) ( 13.789, 14.538 ), Pull ( 15.888, 15.617 ) ( 14.388, 16.936 ) ]
                    |> filled black
                , curve ( -9.892, 24.43 ) [ Pull ( -7.494, 23.081 ) ( -5.096, 21.733 ), Pull ( -3.297, 22.032 ) ( -1.498, 22.332 ), Pull ( -1.199, 22.032 ) ( -0.899, 21.733 ), Pull ( 0.4105, 24.06 ) ( -0.599, 27.428 ), Pull ( -0.717, 30.326 ) ( -4.796, 31.625 ), Pull ( -6.744, 31.815 ) ( -8.693, 30.725 ), Pull ( -8.632, 31.175 ) ( -9.292, 31.625 ), Pull ( -9.812, 31.375 ) ( -9.292, 30.126 ), Pull ( -9.872, 30.076 ) ( -9.892, 29.227 ), Pull ( -10.93, 30.706 ) ( -11.09, 31.625 ), Pull ( -11.29, 30.586 ) ( -10.49, 28.627 ), Pull ( -10.75, 28.178 ) ( -10.49, 27.128 ), Pull ( -11.47, 27.728 ) ( -11.69, 27.728 ), Pull ( -11.66, 26.658 ) ( -10.79, 26.229 ), Pull ( -10.83, 25.18 ) ( -9.892, 24.43 ) ]
                    |> filled black
                , curve ( -9.592, 23.831 ) [ Pull ( -10.29, 30.208 ) ( -4.796, 30.426 ), Pull ( -4.196, 25.18 ) ( -3.597, 19.934 ), Pull ( -8.254, 20.272 ) ( -9.592, 23.831 ) ]
                    |> filled white
                , curve ( -4.796, 30.426 ) [ Pull ( -6.675, 29.657 ) ( -7.194, 27.728 ), Pull ( -7.044, 25.33 ) ( -6.894, 22.932 ), Pull ( -4.796, 22.182 ) ( -2.697, 21.433 ), Pull ( -2.847, 25.33 ) ( -2.997, 29.227 ), Pull ( -3.896, 29.826 ) ( -4.796, 30.426 ) ]
                    |> filled black
                , curve ( -6.594, 29.227 ) [ Pull ( -8.614, 26.849 ) ( -7.194, 22.032 ), Pull ( -5.226, 19.263 ) ( -2.697, 19.934 ), Pull ( -1.118, 20.383 ) ( -0.899, 22.032 ), Pull ( -0.049, 24.131 ) ( -0.599, 26.229 ), Pull ( -0.799, 25.629 ) ( -1.798, 25.03 ), Pull ( -2.327, 23.111 ) ( -3.896, 22.632 ), Pull ( -6.125, 23.511 ) ( -6.594, 25.629 ), Pull ( -7.684, 27.238 ) ( -6.594, 29.227 ) ]
                    |> filled (rgb 128 133 191)
                , curve ( -2.697, 25.629 ) [ Pull ( -3.428, 25.18 ) ( -2.398, 24.73 ), Pull ( -1.717, 25.48 ) ( -2.697, 25.629 ) ]
                    |> filled white
                , curve ( -5.096, 28.627 ) [ Pull ( -6.436, 28.268 ) ( -5.096, 27.428 ), Pull ( -3.265, 28.468 ) ( -5.096, 28.627 ) ]
                    |> filled white
                , curve ( -7.793, 19.934 ) [ Pull ( -7.644, 19.334 ) ( -7.494, 18.735 ), Pull ( -6.295, 19.035 ) ( -5.096, 19.334 ), Pull ( -3.027, 18.745 ) ( -2.398, 16.637 ), Pull ( -5.664, 12.246 ) ( -10.49, 16.936 ), Pull ( -11.21, 19.195 ) ( -7.793, 19.934 ) ]
                    |> filled (rgb 222 93 170)
                , curve ( -14.38, 47.812 ) [ Pull ( -8.564, 52.23 ) ( -0.899, 52.608 ), Pull ( 1.7985, 52.719 ) ( 4.4964, 51.709 ), Pull ( 7.9932, 47.812 ) ( 12.889, 43.915 ), Pull ( 17.866, 41.675 ) ( 20.683, 43.915 ), Pull ( 22.722, 44.444 ) ( 23.081, 46.613 ), Pull ( 23.843, 39.917 ) ( 18.285, 37.021 ), Pull ( 11.69, 34.709 ) ( 5.096, 40.318 ), Pull ( 0.8992, 43.615 ) ( -3.297, 46.913 ), Pull ( -7.943, 45.864 ) ( -12.59, 44.814 ), Pull ( -13.63, 46.463 ) ( -14.38, 47.812 ) ]
                    |> filled (rgb 30 189 125)
                    |> addOutline (solid 0.5) black
                , curve ( 2.0983, 40.618 ) [ Pull ( 3.4473, 43.466 ) ( 4.7962, 46.313 ), Pull ( 0.7494, 48.353 ) ( -3.297, 48.112 ), Pull ( -8.293, 47.753 ) ( -12.29, 45.714 ), Pull ( -11.69, 44.365 ) ( -11.09, 43.016 ), Pull ( -4.496, 41.967 ) ( 2.0983, 40.618 ) ]
                    |> filled (rgb 237 235 81)
                , curve ( 0.2997, 44.215 ) [ Pull ( 5.9449, 52.959 ) ( 12.59, 60.702 ), Pull ( 10.113, 49.97 ) ( 4.7962, 39.718 ), Pull ( 3.4473, 39.569 ) ( 2.0983, 39.419 ), Pull ( 1.0491, 41.817 ) ( 0.2997, 44.215 ) ]
                    |> filled white
                    |> addOutline (solid 0.5) black
                , curve ( 3.2974, 49.011 ) [ Pull ( 3.6861, 45.453 ) ( 6.5948, 43.615 ) ]
                    |> outlined (solid 0.5) black
                , curve ( 5.9953, 52.908 ) [ Pull ( 6.0543, 49.9 ) ( 8.3934, 47.812 ) ]
                    |> outlined (solid 0.5) black
                , curve ( 8.6932, 56.805 ) [ Pull ( 8.9325, 53.967 ) ( 10.491, 52.608 ) ]
                    |> outlined (solid 0.5) black
                , curve ( -42.56, -0.749 ) [ Pull ( -42.95, 5.4171 ) ( -48.26, 7.9437 ), Pull ( -54.1, 12.642 ) ( -59.35, 11.54 ), Pull ( -67.15, 10.793 ) ( -71.64, 4.0468 ), Pull ( -76.95, -3.706 ) ( -70.74, -13.93 ), Pull ( -67.66, -21.03 ) ( -70.74, -24.73 ), Pull ( -75.27, -23.99 ) ( -77.03, -19.33 ), Pull ( -78.63, -24.91 ) ( -74.34, -29.52 ), Pull ( -67.21, -33.04 ) ( -65.04, -26.52 ), Pull ( -64.07, -25.92 ) ( -64.74, -25.33 ) ]
                    |> outlined (solid 0.5) black
                , curve ( -68.34, -30.12 ) [ Pull ( -73.77, -34.41 ) ( -78.83, -29.22 ), Pull ( -78.19, -34.9 ) ( -73.14, -37.62 ), Pull ( -70.55, -38.59 ) ( -68.64, -38.51 ), Pull ( -64.18, -37.98 ) ( -61.45, -35.52 ) ]
                    |> outlined (solid 0.5) black
                , curve ( -68.34, -38.51 ) [ Pull ( -66.54, -40.54 ) ( -61.75, -40.01 ), Pull ( -54.37, -38.89 ) ( -52.15, -31.32 ), Pull ( -49.62, -23.05 ) ( -52.45, -15.73 ), Pull ( -54.13, -9.042 ) ( -52.45, -4.346 ), Pull ( -51.81, -2.907 ) ( -50.06, -2.548 ), Pull ( -48.56, -2.637 ) ( -47.06, -4.046 ) ]
                    |> outlined (solid 0.5) black
                , curve ( -11.99, 43.016 ) [ Pull ( -9.193, 44.925 ) ( -3.597, 44.515 ), Pull ( 4.1395, 42.927 ) ( 4.7962, 37.62 ), Pull ( 6.1268, 33.493 ) ( 3.2974, 30.725 ), Pull ( 3.6096, 35.902 ) ( -2.398, 39.119 ), Pull ( -5.595, 41.118 ) ( -10.19, 40.918 ), Pull ( -10.64, 41.967 ) ( -11.09, 43.016 ), Pull ( -11.24, 43.316 ) ( -11.99, 43.016 ) ]
                    |> filled (rgb 237 81 180)
                    |> addOutline (solid 0.5) black
                , curve ( 11.99, 24.131 ) [ Pull ( 10.791, 24.58 ) ( 9.5925, 25.03 ), Pull ( 8.4318, 30.317 ) ( 11.391, 32.524 ), Pull ( 12.44, 33.514 ) ( 14.088, 34.023 ), Pull ( 14.259, 33.423 ) ( 13.189, 32.824 ), Pull ( 14.238, 32.814 ) ( 15.288, 31.925 ), Pull ( 16.647, 31.614 ) ( 17.086, 32.824 ), Pull ( 17.637, 31.374 ) ( 16.187, 31.325 ), Pull ( 16.936, 30.895 ) ( 17.686, 31.625 ), Pull ( 17.596, 29.905 ) ( 16.187, 30.426 ), Pull ( 15.437, 30.316 ) ( 14.688, 30.725 ), Pull ( 14.388, 28.028 ) ( 14.088, 25.33 ), Pull ( 12.889, 24.88 ) ( 11.99, 24.131 ) ]
                    |> filled black
                , curve ( 12.59, 31.625 ) [ Pull ( 13.489, 31.785 ) ( 14.388, 31.025 ), Pull ( 15.959, 27.288 ) ( 13.489, 23.831 ), Pull ( 12.78, 22.691 ) ( 11.391, 22.632 ), Pull ( 9.7318, 22.35 ) ( 9.5925, 26.829 ), Pull ( 9.4718, 25.719 ) ( 11.391, 25.33 ), Pull ( 12.39, 25.699 ) ( 12.59, 26.829 ), Pull ( 13.189, 26.978 ) ( 13.789, 27.128 ), Pull ( 14.189, 29.797 ) ( 12.59, 31.625 ) ]
                    |> filled (rgb 128 133 191)
                , curve ( 10.791, 30.126 ) [ Pull ( 9.4814, 28.327 ) ( 11.091, 27.728 ), Pull ( 14.031, 29.017 ) ( 10.791, 30.126 ) ]
                    |> filled white
                , curve ( 13.189, 26.829 ) [ Pull ( 12.349, 27.128 ) ( 13.189, 27.428 ), Pull ( 14.239, 26.328 ) ( 13.189, 26.829 ) ]
                    |> filled white
                , curve ( 5.3957, 13.039 ) [ Pull ( 10.641, 9.2292 ) ( 15.887, 14.538 ) ]
                    |> outlined (solid 0.5) black
                , curve ( -22.78, 39.718 ) [ Pull ( -25.46, 34.213 ) ( -26.37, 27.428 ), Pull ( -27.34, 24.189 ) ( -31.47, 24.43 ), Pull ( -35.63, 25.248 ) ( -35.67, 29.826 ), Pull ( -39.07, 21.511 ) ( -31.47, 18.435 ), Pull ( -30.12, 17.945 ) ( -29.37, 18.735 ), Pull ( -30.41, 15.316 ) ( -35.37, 15.737 ), Pull ( -38.67, 17.395 ) ( -40.16, 20.533 ), Pull ( -40.24, 6.6911 ) ( -29.67, 2.2482 ), Pull ( -27.38, 1.928 ) ( -25.77, 2.8477 ), Pull ( -21.9, 6.9437 ) ( -23.98, 13.039 ), Pull ( -25.4, 18.645 ) ( -25.18, 24.73 ), Pull ( -23.85, 30.606 ) ( -20.68, 36.121 ), Pull ( -21.69, 37.8 ) ( -22.78, 39.718 ) ]
                    |> filled (rgb 237 81 180)
                    |> addOutline (solid 0.5) black
                , curve ( -22.18, 11.241 ) [ Pull ( -17.98, 10.491 ) ( -13.78, 9.7423 ), Pull ( -10.15, 2.548 ) ( -13.48, -4.646 ), Pull ( -15.03, -11.4 ) ( -25.18, -12.44 ), Pull ( -29.81, -11.99 ) ( -32.37, -9.142 ), Pull ( -25.85, -9.965 ) ( -23.38, -3.147 ), Pull ( -22.44, -1.498 ) ( -22.78, 0.1498 ), Pull ( -26.22, -2.269 ) ( -29.67, -1.648 ), Pull ( -25.99, 0.0494 ) ( -24.28, 3.1475 ), Pull ( -23.23, 7.1943 ) ( -22.18, 11.241 ) ]
                    |> filled (rgb 19 72 176)
                    |> addOutline (solid 0.5) black
                , curve ( -19.18, 34.622 ) [ Pull ( -17.98, 33.403 ) ( -16.78, 33.423 ), Pull ( -21.69, 25.613 ) ( -13.48, 9.4426 ), Pull ( -12.71, 4.3861 ) ( -15.58, 0.4496 ), Pull ( -15.56, 4.6463 ) ( -17.38, 8.843 ), Pull ( -16.68, 4.4964 ) ( -19.18, 0.1498 ), Pull ( -19.48, 4.6463 ) ( -21.58, 9.1428 ), Pull ( -20.64, 3.0463 ) ( -26.97, 0.1498 ), Pull ( -21.66, 5.2459 ) ( -23.38, 10.341 ), Pull ( -24.31, 14.538 ) ( -24.88, 18.735 ), Pull ( -23.83, 26.679 ) ( -19.18, 34.622 ) ]
                    |> filled (rgb 30 131 189)
                , curve ( -20.38, 35.822 ) [ Pull ( -27.44, 27.69 ) ( -23.98, 13.639 ), Pull ( -25.01, 27.451 ) ( -18.88, 34.622 ), Pull ( -19.85, 35.092 ) ( -20.38, 35.822 ) ]
                    |> filled (rgb 237 235 81)
                    |> addOutline (solid 0.5) black
                , curve ( -9.892, 24.43 ) [ Pull ( -8.543, 22.401 ) ( -7.194, 21.733 ) ]
                    |> outlined (solid 0.4) black
                , curve ( -10.19, 25.629 ) [ Pull ( -10.34, 24.88 ) ( -9.892, 24.131 ) ]
                    |> outlined (solid 0.4) black
                ]
                |> scale 0.55
                |> move ( 20, -5 )

        -- Dirt spots: (id, x, y, kind)  0=pebble  1=stick  2=mud
        -- Positions are in collage coordinates (matching spotPositions)
        allSpotData : List Spot
        allSpotData =
            [ { id = 1, x = 5, y = -10, kind = 0 }
            , { id = 2, x = 18, y = 5, kind = 1 }
            , { id = 3, x = 16, y = -16, kind = 0 }
            , { id = 4, x = 0, y = 10, kind = 1 }
            , { id = 5, x = 12, y = -5, kind = 0 }
            , { id = 6, x = 28, y = 8, kind = 1 }
            , { id = 7, x = 0, y = -20, kind = 2 }
            , { id = 8, x = -10, y = -12, kind = 2 }
            , { id = 9, x = -18, y = -5, kind = 2 }
            , { id = 10, x = 10, y = 10, kind = 2 }
            , { id = 11, x = 0, y = 7, kind = 2 }
            , { id = 12, x = -10, y = 0, kind = 2 }
            ]

        drawSpot spot =
            let
                id =
                    spot.id

                sx =
                    spot.x

                sy =
                    spot.y

                kind =
                    spot.kind
            in
            if kind == 2 then
                -- mud: stays until washing phase cleans it
                if isCleaned id model.washCleaned then
                    group []

                else
                    group
                        [ oval 5 3.5 |> filled (rgb 110 80 50)
                        , oval 3 2 |> filled (rgb 130 95 60) |> move ( -0.5, 0.3 )
                        , oval 2 1.5 |> filled (rgb 110 80 50) |> move ( 2, -1 )
                        ]
                        |> move ( sx, sy )

            else
            -- pebbles/sticks: disappear when brushed
            if
                isCleaned id model.cleaned
            then
                group []

            else
                (case kind of
                    0 ->
                        group
                            [ oval 4 3 |> filled (rgb 140 130 120) |> addOutline (solid 0.5) (rgb 80 70 60)
                            , oval 2 1.5 |> filled (rgb 170 160 150) |> move ( -0.5, 0.5 )
                            ]

                    _ ->
                        group
                            [ rect 7 2 |> filled (rgb 120 80 40) |> rotate (degrees 30) |> addOutline (solid 0.5) (rgb 80 50 20)
                            , rect 3 1.5 |> filled (rgb 150 100 60) |> rotate (degrees 30) |> move ( 1, 1 )
                            ]
                )
                    |> move ( sx, sy )

        dirtLayer =
            case phase of
                Intro ->
                    List.map drawSpot allSpotData

                Brushing ->
                    List.map drawSpot allSpotData

                PostBrush ->
                    List.map drawSpot allSpotData

                Washing ->
                    List.map drawSpot allSpotData

                _ ->
                    []

        allWashSpotData : List Spot
        allWashSpotData =
            [ { id = 1, x = 6, y = -10, kind = 2 }
            , { id = 2, x = 0, y = 5, kind = 2 }
            , { id = 3, x = -10, y = -20, kind = 2 }
            , { id = 4, x = 0, y = 10, kind = 2 }
            , { id = 5, x = -10, y = -5, kind = 2 }
            , { id = 6, x = 0, y = 8, kind = 2 }
            ]

        {- dirtLayer =
           if phase == Brushing then
             List.map drawSpot allSpotData
           else if phase == Washing then
             List.map drawSpot allWashSpotData
           else
             []
        -}
        bubbles =
            if phase == Washing then
                [ -- Large bubble 1
                  group
                    [ circle 5 |> filled (rgba 180 220 255 0.08)
                    , circle 5 |> outlined (solid 0.8) (rgba 200 235 255 0.6)
                    , circle 2 |> filled (rgba 255 255 255 0.5) |> move ( -1.5, 2 )
                    , oval 3 1.5 |> filled (rgba 255 255 255 0.3) |> move ( -1, 2.5 )
                    , circle 0.8 |> filled (rgba 255 255 255 0.9) |> move ( -2, 2.8 )
                    , circle 4.2 |> outlined (solid 0.4) (rgba 150 200 255 0.3)
                    ]
                    |> move ( 25 + 5 * sin model.time, 10 + 8 * sin (model.time * 1.2) )
                , -- Small bubble 1
                  group
                    [ circle 3 |> filled (rgba 180 220 255 0.08)
                    , circle 3 |> outlined (solid 0.7) (rgba 200 235 255 0.55)
                    , circle 1.2 |> filled (rgba 255 255 255 0.5) |> move ( -0.8, 1.2 )
                    , oval 1.8 0.9 |> filled (rgba 255 255 255 0.3) |> move ( -0.7, 1.5 )
                    , circle 0.5 |> filled (rgba 255 255 255 0.9) |> move ( -1.2, 1.8 )
                    ]
                    |> move ( 15 + 4 * cos model.time, 5 + 10 * sin (model.time * 0.9) )
                , -- Large bubble 2
                  group
                    [ circle 6 |> filled (rgba 180 220 255 0.07)
                    , circle 6 |> outlined (solid 0.9) (rgba 210 240 255 0.55)
                    , circle 2.5 |> filled (rgba 255 255 255 0.45) |> move ( -2, 2.5 )
                    , oval 3.5 1.8 |> filled (rgba 255 255 255 0.25) |> move ( -1.5, 3 )
                    , circle 1 |> filled (rgba 255 255 255 0.9) |> move ( -2.5, 3.5 )
                    , circle 5 |> outlined (solid 0.4) (rgba 180 220 255 0.25)
                    , circle 0.6 |> filled (rgba 200 230 255 0.7) |> move ( 2, -2 )
                    ]
                    |> move ( 35 + 3 * sin (model.time * 1.5), 15 + 6 * cos model.time )
                , -- Tiny bubble cluster
                  group
                    [ circle 1.8 |> filled (rgba 180 220 255 0.1)
                    , circle 1.8 |> outlined (solid 0.6) (rgba 200 235 255 0.6)
                    , circle 0.7 |> filled (rgba 255 255 255 0.6) |> move ( -0.5, 0.8 )
                    ]
                    |> move ( 10 + 6 * cos (model.time * 1.1), 20 + 4 * sin model.time )
                , -- Medium bubble
                  group
                    [ circle 4 |> filled (rgba 190 225 255 0.07)
                    , circle 4 |> outlined (solid 0.8) (rgba 200 235 255 0.5)
                    , circle 1.6 |> filled (rgba 255 255 255 0.5) |> move ( -1.2, 1.8 )
                    , oval 2.2 1.1 |> filled (rgba 255 255 255 0.28) |> move ( -1, 2.2 )
                    , circle 0.6 |> filled (rgba 255 255 255 0.85) |> move ( -1.8, 2.6 )
                    , circle 3.2 |> outlined (solid 0.35) (rgba 160 210 255 0.3)
                    ]
                    |> move ( 40 + 4 * sin model.time, 0 + 7 * cos (model.time * 0.8) )
                , -- Extra small drifting bubble
                  group
                    [ circle 2.2 |> filled (rgba 180 220 255 0.09)
                    , circle 2.2 |> outlined (solid 0.6) (rgba 210 240 255 0.6)
                    , circle 0.9 |> filled (rgba 255 255 255 0.55) |> move ( -0.6, 1 )
                    , circle 0.4 |> filled (rgba 255 255 255 0.9) |> move ( -0.9, 1.4 )
                    ]
                    |> move ( 20 + 7 * cos (model.time * 0.7), -5 + 5 * sin (model.time * 1.3) )
                , -- Rainbow-tint bubble (iridescent effect)
                  group
                    [ circle 4.5 |> filled (rgba 180 220 255 0.07)
                    , circle 4.5 |> outlined (solid 0.8) (rgba 220 200 255 0.5)
                    , circle 4.5 |> outlined (solid 0.4) (rgba 200 255 220 0.25) |> move ( 0.5, 0 )
                    , circle 1.8 |> filled (rgba 255 255 255 0.45) |> move ( -1.5, 2 )
                    , oval 2.5 1.2 |> filled (rgba 255 255 255 0.25) |> move ( -1.2, 2.4 )
                    , circle 0.7 |> filled (rgba 255 255 255 0.9) |> move ( -2, 2.8 )
                    ]
                    |> move ( 5 + 6 * sin (model.time * 0.85), 12 + 9 * cos (model.time * 1.05) )
                ]

            else
                []

        sparkles =
            if phase == Done then
                [ {- circle 2 |> filled (rgb 255 215 0) |> move (20 * cos model.time, 20 * sin model.time) |> move (20, 10)
                     , circle 1.5 |> filled (rgb 255 100 200) |> move (15 * cos (model.time + 2), 15 * sin (model.time + 2)) |> move (20, 10)
                     , circle 2 |> filled (rgb 100 255 200) |> move (18 * cos (model.time + 4), 18 * sin (model.time + 4)) |> move (20, 10)
                     , circle 1.5 |> filled (rgb 255 255 100) |> move (22 * cos (model.time + 1), 22 * sin (model.time + 1)) |> move (20, 10)
                  -}
                  sparkles2
                    |> move ( -20, 10 )
                    |> makeTransparent (sin (1.5 * model.time))
                , sparkles3
                    |> makeTransparent (sin model.time)
                ]

            else
                []

        brushTool =
            group
                [ rect 17 4 |> filled (rgb 180 120 60)
                , rect 12 6 |> filled (rgb 220 200 160) |> move ( 0, -4 )
                , rect 1 5 |> filled (rgb 100 80 40) |> move ( -5, -4 )
                , rect 1 5 |> filled (rgb 100 80 40) |> move ( -3, -4 )
                , rect 1 5 |> filled (rgb 100 80 40) |> move ( -1, -4 )
                , rect 1 5 |> filled (rgb 100 80 40) |> move ( 1, -4 )
                , rect 1 5 |> filled (rgb 100 80 40) |> move ( 3, -4 )
                , rect 1 5 |> filled (rgb 100 80 40) |> move ( 5, -4 )
                ]

        bucketShape =
            group
                [ -- bucket handle
                  {- circle 7 |> outlined (solid 2) (rgb 201 201 201) |> move (-55, -38)

                     -- Water bucket
                     , rect 14 12 |> filled (rgb 50 80 130) |> move (-55, -47)
                     , rect 16 3 |> filled (rgb 40 65 110) |> move (-55, -41)
                  -}
                  bucket
                    |> scale 0.07
                ]

        treatShape =
            group
                [ curve ( -39.56, 25.929 ) [ Pull ( -17.23, 33.03 ) ( -6.295, 24.73 ), Pull ( 11.649, 9.0941 ) ( 8.3934, -9.742 ), Pull ( 5.8498, -29.77 ) ( -8.093, -42.41 ), Pull ( -14.98, -49.21 ) ( -21.88, -49.61 ), Pull ( -29.37, -49.86 ) ( -36.87, -48.71 ), Pull ( -40.31, -48.26 ) ( -43.76, -49.01 ), Pull ( -48.11, -50.11 ) ( -52.45, -49.01 ), Pull ( -55.9, -47.81 ) ( -59.35, -45.41 ), Pull ( -71.84, -34.57 ) ( -76.74, -20.53 ), Pull ( -82.99, -2.497 ) ( -75.84, 13.939 ), Pull ( -70.64, 25.083 ) ( -59.05, 28.627 ), Pull ( -54.85, 29.728 ) ( -46.46, 26.829 ), Pull ( -43.01, 25.829 ) ( -39.56, 25.929 ) ]
                    |> filled red
                    |> scale 0.17
                , curve ( -8.093, 19.035 ) [ Pull ( 7.2023, 3.6489 ) ( 2.0983, -16.33 ), Pull ( 12.352, 4.9988 ) ( -8.093, 19.035 ) ]
                    |> filled white
                    |> scale 0.17
                , curve ( -44.96, 45.114 ) [ Pull ( -39.71, 36.572 ) ( -38.66, 26.229 ), Pull ( -37.77, 26.529 ) ( -36.87, 26.829 ), Pull ( -35.02, 36.121 ) ( -37.77, 45.414 ), Pull ( -41.36, 45.114 ) ( -44.96, 45.114 ) ]
                    |> filled darkBrown
                    |> scale 0.17
                , curve ( -35.67, 34.622 ) [ Pull ( -22.63, 55.616 ) ( -1.199, 50.21 ), Pull ( -12.23, 28.216 ) ( -35.67, 34.622 ) ]
                    |> filled darkGreen
                    |> scale 0.17
                , curve ( -35.67, 34.922 ) [ Pull ( -22.63, 43.618 ) ( -9.592, 46.313 ) ]
                    |> outlined (solid 0.25) black
                    |> scale 0.17
                ]

        shelfLeft =
            [ rect 30 3 |> filled (rgb 120 80 30) |> move ( -80, 10 )
            , case phase of
                Brushing ->
                    group []

                _ ->
                    brushTool |> move ( -80, 18 ) |> notifyTap PickBrush
            ]

        shelfRight =
            [ rect 30 3 |> filled (rgb 120 80 30) |> move ( 80, 10 )
            , case phase of
                Washing ->
                    group []

                _ ->
                    bucketShape |> move ( 40, -33 ) |> notifyTap PickBucket
            , case phase of
                Treating ->
                    group []

                _ ->
                    treatShape |> move ( 85, 20 ) |> notifyTap PickTreat
            ]

        -- Invisible full-screen rect catches all mouse moves when holding a tool.
        -- The tool shape is drawn on top of it at the current cursor position.
        heldTool =
            case phase of
                Brushing ->
                    group
                        [ rect 192 128 |> filled (rgba 0 0 0 0) |> notifyMouseMoveAt MoveTool
                        , brushTool |> move ( model.toolX, model.toolY )
                        ]

                Washing ->
                    group
                        [ rect 192 128 |> filled (rgba 0 0 0 0) |> notifyMouseMoveAt MoveTool
                        , bucketShape |> move ( model.toolX, model.toolY )
                        ]

                Treating ->
                    group
                        [ rect 192 128 |> filled (rgba 0 0 0 0) |> notifyMouseMoveAt MoveTool
                        , treatShape |> move ( model.toolX, model.toolY ) |> notifyTap Feed
                        ]

                _ ->
                    group []

        progressBar =
            group
                [ rect 70 12 |> filled (rgba 0 0 0 0.3) |> move ( 0, 57 )
                , text "Brush"
                    |> size 7
                    |> filled
                        (if model.brushed then
                            rgb 80 255 120

                         else
                            rgb 180 180 180
                        )
                    |> move ( -33, 54 )
                , text "Wash"
                    |> size 7
                    |> filled
                        (if model.washed then
                            rgb 80 255 120

                         else
                            rgb 180 180 180
                        )
                    |> move ( -8, 54 )
                , text "Treat"
                    |> size 7
                    |> filled
                        (if model.treated then
                            rgb 80 255 120

                         else
                            rgb 180 180 180
                        )
                    |> move ( 15, 54 )
                ]

        dirtCounter =
            let
                remaining =
                    if phase == Washing then
                        6 - List.length model.washCleaned

                    else
                        6 - List.length model.cleaned
            in
            if phase == Brushing || phase == Washing then
                group
                    [ rect 52 12 |> filled (rgba 0 0 0 0.4) |> move ( 60, 57 )
                    , text (String.fromInt remaining ++ " spots left") |> size 6 |> filled white |> move ( 37, 54 )
                    ]

            else
                group []

        instructionText =
            case phase of
                Intro ->
                    group
                        [ rect 180 18 |> filled (rgba 0 0 0 0.5) |> move ( 0, -52 )
                        , text "Uh oh... Click on the BRUSH on the left to help Stardust get ready!" |> size 6 |> filled white |> move ( -80, -55 )
                        ]

                Brushing ->
                    group
                        [ rect 180 18 |> filled (rgba 0 0 0 0.5) |> move ( 0, -52 )
                        , text "Let's brush out the sticks and pebbles!" |> size 6 |> filled white |> move ( -50, -55 )
                        ]

                PostBrush ->
                    group
                        [ rect 180 18 |> filled (rgba 0 0 0 0.5) |> move ( 0, -52 )
                        , text "Great brushing! Now click the BUCKET on the right to wash!" |> size 6 |> filled white |> move ( -75, -55 )
                        ]

                Washing ->
                    group
                        [ rect 180 18 |> filled (rgba 0 0 0 0.5) |> move ( 0, -52 )
                        , text "Scrub every dirty spot away with the sponge!" |> size 6 |> filled white |> move ( -55, -55 )
                        ]

                PostWash ->
                    group
                        [ rect 180 18 |> filled (rgba 0 0 0 0.5) |> move ( 0, -52 )
                        , text "Sparkling clean! Grab the TREAT on the right shelf!" |> size 6 |> filled white |> move ( -67, -55 )
                        ]

                Treating ->
                    group
                        [ rect 180 18 |> filled (rgba 0 0 0 0.5) |> move ( 0, -52 )
                        , text "Click Stardust to give her the treat!" |> size 6 |> filled white |> move ( -45, -55 )
                        ]

                Done ->
                    group
                        [ rect 180 18 |> filled (rgba 0 0 0 0.5) |> move ( 0, -52 )
                        , text "Stardust is so happy! You are an amazing unicorn groomer!" |> size 6 |> filled (rgb 255 215 0) |> move ( -69, -55 )
                        ]

        heartDone =
            if phase == Done then
                group
                    [{- circle 5 |> filled (rgb 255 100 150) |> move (55, 35)
                        , text "<3" |> size 10 |> filled (rgb 255 100 150) |> move (50, 32)
                     -}
                    ]

            else
                group []

        happyOverlay =
            if phase == Done then
                group []
                {- group
                   [ curve (8, 18) [Pull (12, 14) (16, 18)] |> outlined (solid 1.5) black |> move (20, -5)
                   ]
                -}

            else
                group []
    in
    List.concat
        [ stableBg

        --, hay
        , windowPane
        , stars
        , [ unicornGroup ]
        , dirtLayer
        , bubbles
        , sparkles
        , shelfLeft
        , shelfRight
        , [ heldTool ]
        , [ progressBar ]
        , [ dirtCounter ]
        , [ instructionText ]
        , [ heartDone ]
        , [ happyOverlay ]
        ]


type Phase
    = Intro
    | Brushing
    | PostBrush
    | Washing
    | PostWash
    | Treating
    | Done


type Msg
    = Tick Float GetKeyState
    | PickBrush
    | PickBucket
    | PickTreat
    | MoveTool ( Float, Float )
    | Feed


type alias Model =
    { time : Float
    , phase : Phase
    , toolX : Float
    , toolY : Float
    , cleaned : List Int
    , washCleaned : List Int
    , brushed : Bool
    , washed : Bool
    , treated : Bool
    }


type alias Spot =
    { id : Int
    , x : Float
    , y : Float
    , kind : Int
    }


update msg model =
    case msg of
        Tick t _ ->
            { model | time = t }

        PickBrush ->
            case model.phase of
                Intro ->
                    { model | phase = Brushing }

                _ ->
                    model

        PickBucket ->
            case model.phase of
                PostBrush ->
                    { model | phase = Washing, washCleaned = [] }

                _ ->
                    model

        PickTreat ->
            case model.phase of
                PostWash ->
                    { model | phase = Treating }

                _ ->
                    model

        MoveTool ( x, y ) ->
            let
                baseModel =
                    { model | toolX = x, toolY = y }

                cleanSpot ( id, sx, sy ) acc =
                    if not (List.member id acc) && nearSpot x y sx sy 8 then
                        id :: acc

                    else
                        acc
            in
            case model.phase of
                Brushing ->
                    let
                        newCleaned =
                            List.foldl cleanSpot model.cleaned spotPositions
                    in
                    if List.length newCleaned == 6 then
                        { baseModel | cleaned = newCleaned, phase = PostBrush, brushed = True }

                    else
                        { baseModel | cleaned = newCleaned }

                Washing ->
                    let
                        newWashCleaned =
                            List.foldl cleanSpot model.washCleaned washSpotPositions
                    in
                    if List.length newWashCleaned == 6 then
                        { baseModel | washCleaned = newWashCleaned, phase = PostWash, washed = True }

                    else
                        { baseModel | washCleaned = newWashCleaned }

                _ ->
                    baseModel

        Feed ->
            case model.phase of
                Treating ->
                    { model | phase = Done, treated = True }

                _ ->
                    model


bucket =
    group
        [ -- Bucket body
          polygon [ ( -110, 120 ), ( 110, 120 ), ( 80, -120 ), ( -80, -120 ) ]
            |> filled (rgb 74 144 196)
        , polygon [ ( -110, 120 ), ( 110, 120 ), ( 80, -120 ), ( -80, -120 ) ]
            |> outlined (solid 2.5) (rgb 44 100 147)

        -- Water fill
        , polygon [ ( -105, 80 ), ( 105, 80 ), ( 78, -118 ), ( -78, -118 ) ]
            |> filled (rgb 91 190 240)

        -- Water surface shimmer
        , polygon [ ( -105, 80 ), ( 105, 80 ), ( 105, 65 ), ( -105, 65 ) ]
            |> filled (rgb 125 212 248)

        -- Bucket shine (left side)
        , polygon [ ( -100, 110 ), ( -75, 110 ), ( -90, -110 ), ( -110, -110 ) ]
            |> filled (rgba 142 200 232 0.35)

        -- Bucket rim
        , roundedRect 220 18 1
            |> filled (rgb 44 100 147)
            |> move ( 0, 120 )

        -- Bucket base
        , roundedRect 160 14 1
            |> filled (rgb 44 100 147)
            |> move ( 0, -120 )

        -- Handle arc
        , curve ( 0, 170 )
            [ Pull ( 0, 210 ) ( -110, 130 )
            , Pull ( 0, 210 ) ( 110, 130 )
            ]
            |> outlined (solid 5) (rgb 44 100 147)

        -- Handle knobs
        , circle 7
            |> filled (rgb 44 100 147)
            |> move ( -110, 125 )
        , circle 7
            |> filled (rgb 44 100 147)
            |> move ( 110, 125 )

        -- Sponge body
        , roundedRect 100 58 1
            |> filled (rgb 232 200 50)
            |> addOutline (solid 1.5) (rgb 184 154 16)
            |> move ( 0, 40 )

        -- Sponge wet overlay
        , roundedRect 100 28 1
            |> filled (rgba 91 190 240 0.35)
            |> move ( 0, 25 )

        -- Sponge pores
        , oval 10 8
            |> filled (rgb 201 168 32)
            |> move ( -38, 55 )
        , oval 8 6
            |> filled (rgb 201 168 32)
            |> move ( -20, 43 )
        , oval 10 8
            |> filled (rgb 201 168 32)
            |> move ( 5, 57 )
        , oval 8 7
            |> filled (rgb 201 168 32)
            |> move ( 22, 45 )
        , oval 10 8
            |> filled (rgb 201 168 32)
            |> move ( 40, 56 )
        , oval 8 6
            |> filled (rgb 201 168 32)
            |> move ( -28, 35 )
        , oval 9 7
            |> filled (rgb 201 168 32)
            |> move ( 18, 37 )
        , oval 8 6
            |> filled (rgb 201 168 32)
            |> move ( 45, 38 )

        -- Bubbles
        , circle 5
            |> outlined (solid 1.2) (rgba 174 228 248 0.7)
            |> move ( -70, 60 )
        , circle 4
            |> outlined (solid 1.2) (rgba 174 228 248 0.7)
            |> move ( 80, 68 )
        , circle 3
            |> outlined (solid 1.2) (rgba 174 228 248 0.7)
            |> move ( 68, 50 )
        , circle 3.5
            |> outlined (solid 1.2) (rgba 174 228 248 0.7)
            |> move ( -65, 45 )
        , circle 2.5
            |> outlined (solid 1.2) (rgba 174 228 248 0.7)
            |> move ( -80, 70 )

        -- Water ripple lines
        , line ( -90, 73 ) ( -40, 73 )
            |> outlined (solid 1.5) (rgba 174 228 248 0.6)
        , line ( 30, 75 ) ( 80, 75 )
            |> outlined (solid 1.5) (rgba 174 228 248 0.6)
        ]


sparkles2 =
    group
        [ curve ( -31.47, 17.836 ) [ Pull ( -32.62, 13.789 ) ( -36.57, 9.7423 ), Pull ( -32.22, 7.1943 ) ( -32.67, 4.6463 ), Pull ( -29.32, 10.094 ) ( -27.57, 9.1428 ), Pull ( -32.42, 13.489 ) ( -31.47, 17.836 ) ]
            |> filled lightGrey
            |> addOutline (solid 0.25) black
            |> scale 0.6
            |> move ( 0, -40 )
        , curve ( -31.47, 17.836 ) [ Pull ( -32.62, 13.789 ) ( -36.57, 9.7423 ), Pull ( -32.22, 7.1943 ) ( -32.67, 4.6463 ), Pull ( -29.32, 10.094 ) ( -27.57, 9.1428 ), Pull ( -32.42, 13.489 ) ( -31.47, 17.836 ) ]
            |> filled lightGrey
            |> addOutline (solid 0.25) black
            |> scale 0.5
            |> move ( 30, -25 )
        , curve ( -31.47, 17.836 ) [ Pull ( -32.62, 13.789 ) ( -36.57, 9.7423 ), Pull ( -32.22, 7.1943 ) ( -32.67, 4.6463 ), Pull ( -29.32, 10.094 ) ( -27.57, 9.1428 ), Pull ( -32.42, 13.489 ) ( -31.47, 17.836 ) ]
            |> filled lightGrey
            |> addOutline (solid 0.25) black
            |> scale 0.8
            |> move ( 55, -15 )
        ]


sparkles3 =
    group
        [ curve ( -31.47, 17.836 ) [ Pull ( -32.62, 13.789 ) ( -36.57, 9.7423 ), Pull ( -32.22, 7.1943 ) ( -32.67, 4.6463 ), Pull ( -29.32, 10.094 ) ( -27.57, 9.1428 ), Pull ( -32.42, 13.489 ) ( -31.47, 17.836 ) ]
            |> filled lightGrey
            |> addOutline (solid 0.25) black
        , curve ( -31.47, 17.836 ) [ Pull ( -32.62, 13.789 ) ( -36.57, 9.7423 ), Pull ( -32.22, 7.1943 ) ( -32.67, 4.6463 ), Pull ( -29.32, 10.094 ) ( -27.57, 9.1428 ), Pull ( -32.42, 13.489 ) ( -31.47, 17.836 ) ]
            |> filled lightGrey
            |> addOutline (solid 0.25) black
            |> scale 0.7
            |> move ( 20, 20 )
        , curve ( -31.47, 17.836 ) [ Pull ( -32.62, 13.789 ) ( -36.57, 9.7423 ), Pull ( -32.22, 7.1943 ) ( -32.67, 4.6463 ), Pull ( -29.32, 10.094 ) ( -27.57, 9.1428 ), Pull ( -32.42, 13.489 ) ( -31.47, 17.836 ) ]
            |> filled lightGrey
            |> addOutline (solid 0.25) black
            |> scale 0.6
            |> move ( 0, -40 )
        , curve ( -31.47, 17.836 ) [ Pull ( -32.62, 13.789 ) ( -36.57, 9.7423 ), Pull ( -32.22, 7.1943 ) ( -32.67, 4.6463 ), Pull ( -29.32, 10.094 ) ( -27.57, 9.1428 ), Pull ( -32.42, 13.489 ) ( -31.47, 17.836 ) ]
            |> filled lightGrey
            |> addOutline (solid 0.25) black
            |> scale 0.5
            |> move ( 30, -25 )
        , curve ( -31.47, 17.836 ) [ Pull ( -32.62, 13.789 ) ( -36.57, 9.7423 ), Pull ( -32.22, 7.1943 ) ( -32.67, 4.6463 ), Pull ( -29.32, 10.094 ) ( -27.57, 9.1428 ), Pull ( -32.42, 13.489 ) ( -31.47, 17.836 ) ]
            |> filled lightGrey
            |> addOutline (solid 0.25) black
            |> scale 0.8
            |> move ( 55, -15 )
        ]


init =
    { time = 0
    , phase = Intro
    , toolX = 0
    , toolY = 0
    , cleaned = []
    , washCleaned = []
    , brushed = False
    , washed = False
    , treated = False
    }


main =
    gameApp Tick
        { model = init
        , view = view
        , update = update
        , title = "Unicorn Grooming Stable"
        }


view model =
    collage 192 128 (myShapes model)
