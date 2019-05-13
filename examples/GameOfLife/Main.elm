module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import Html exposing (Html)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import CellGrid exposing (CellGrid, CellRenderer)
import Time exposing (Posix)
import Conway exposing(State(..))
import Random
import Color
import Html.Events.Extra.Mouse as Mouse


tickInterval : Float
tickInterval =
    333


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { input : String
    , output : String
    , counter : Int
    , trial : Int
    , appState : AppState
    , density : Float
    , densityString : String
    , currentDensity : Float
    , seed : Int
    , seedString : String
    , randomPair : (Int, Int)
    , heatMap : CellGrid State
    }


type AppState
    = Ready
    | Running
    | Paused

--
-- MSG
--

type Msg
    = NoOp
    | InputBeta String
    | InputSeed String
    | Step
    | Tick Posix
    | AdvanceAppState
    | Reset
    | NewPair (Int, Int)
    -- | MouseClick (Float, Float)


type alias Flags =
    {}


initialDensity = 0.3
initialSeed = 3771
gridWidth = 80
lowDensityThreshold = 0.00

init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = "Test"
      , output = "Test"
      , counter = 0
      , trial = 0
      , appState = Ready
      , density = initialDensity
      , densityString = String.fromFloat initialDensity
      , currentDensity = initialDensity
      , seed = initialSeed
      , seedString  = String.fromInt initialSeed
      , randomPair = (0,0)
      , heatMap = initialCellGrid initialSeed initialDensity
      }
    , Cmd.none
    )

initialCellGrid : Int -> Float -> CellGrid State
initialCellGrid seed density =
    Conway.randomCellGrid seed density ( gridWidth, gridWidth )
--             |> Conway.spot (20,20) 8 Occupied
--             |> Conway.spot (8,8) 8 Unoccupied
--             |> Conway.spot (8,8) 3 Unoccupied


subscriptions model =
    Time.every tickInterval Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputBeta str ->
            case String.toFloat str of
                Nothing ->
                    ( { model | densityString = str }, Cmd.none )

                Just density_ ->
                    ( { model | densityString = str, density = density_ }, Cmd.none )

        InputSeed str ->
            case String.toInt str of
                 Nothing ->
                     ( { model | seedString = str }, Cmd.none )

                 Just seed_ ->
                            ( { model | seedString = str, seed = seed_ }, Cmd.none )

        Step ->
            ( { model | counter = model.counter + 1, heatMap = Conway.updateCells model.heatMap }, Cmd.none )

        Tick t ->
            case model.appState == Running of
                True ->
                    ( { model |
                         counter = model.counter + 1
                        , heatMap = Conway.updateCells  model.heatMap |> generateNewLife model
                        , currentDensity = currentDensity model
                      },
                        Random.generate NewPair generatePair)

                False ->
                    ( model, Cmd.none )

        AdvanceAppState ->
            let
                nextAppState =
                    case model.appState of
                        Ready ->
                            Running

                        Running ->
                            Paused

                        Paused ->
                            Running
            in
                ( { model | appState = nextAppState }, Cmd.none )

        Reset ->
            ( { model | counter = 0, trial = model.trial + 1,
                appState = Ready, heatMap = initialCellGrid (model.seed + model.trial + 1) model.density}, Cmd.none )

        NewPair (i, j) ->
           ({ model | randomPair = (i,j)}, Cmd.none)


generateNewLife : Model -> CellGrid State -> CellGrid State
generateNewLife model cg =
    case model.currentDensity < lowDensityThreshold of
        False -> cg
        True ->
            let
                (i,j) = model.randomPair
            in
                Conway.occupy (i,j) cg
                 |> Conway.occupy (i+1,j)
                 |> Conway.occupy (i+1,j+1)
                 |> Conway.occupy (i+1,j+2)
                 |> Conway.occupy (i+1,j+3)
                 |> Conway.occupy (i+2,j+3)
                 |> Conway.occupy (i+1,j+3)
                 |> Conway.occupy (i,j+3)


generatePair =
    Random.pair (Random.int 0 (gridWidth - 1)) (Random.int 0 (gridWidth - 1))

--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ centerX, spacing 20 ]
            [ title "Conway's Game of Life"
            , el [] (CellGrid.renderAsHtml 400 400 cellrenderer model.heatMap |> Element.html)
            , row [ spacing 18 ]
                [ resetButton
                , runButton model
                , row [ spacing 8 ] [ stepButton, counterDisplay model ]
                , inputDensity model
                ]
            , row [Font.size 14, centerX, spacing 12] [
                 el [ ] (text <| "Current density = " ++ String.fromFloat model.currentDensity)
                 --, el [ ] (text "Run with 0 < density < 1.0")
                ]
            ]
        ]


--onMouseClick : Attribute Msg
--onMouseClick =
--    Mouse.onClick (.clientPos >> MouseClick)


currentDensity : Model -> Float
currentDensity model =
    let
        population = Conway.occupied model.heatMap |> toFloat
        capacity = gridWidth*gridWidth |> toFloat
    in
        population/capacity |> roundTo 4


roundTo : Int -> Float -> Float
roundTo places x =
    let
        k = 10^places |> toFloat
    in
     (round (k*x) |> toFloat)/k


cellrenderer =
    {
         cellSize = 5
       , cellColorizer = \state ->
            case state of
               Occupied -> Color.rgb 0 0 1
               Unoccupied -> Color.rgb 0 0 0
       , defaultColor = Color.rgb 0 0 0
       , gridLineWidth = 0.5
       , gridLineColor = Color.rgb 0 0 1
    }


counterDisplay : Model -> Element Msg
counterDisplay model =
    el [ Font.size 18, width (px 30) ] (text <| String.fromInt model.counter)


title : String -> Element msg
title str =
    row [ centerX, Font.bold ] [ text str ]


outputDisplay : Model -> Element msg
outputDisplay model =
    row [ centerX ]
        [ text model.output ]


buttonFontSize =
    16


inputDensity : Model -> Element Msg
inputDensity model =
    Input.text [ width (px 60), Font.size buttonFontSize ]
        { onChange = InputBeta
        , text = model.densityString
        , placeholder = Nothing
        , label = Input.labelLeft [] <| el [ Font.size buttonFontSize, moveDown 12 ] (text "Initial density ")
        }

inputSeed : Model -> Element Msg
inputSeed model =
    Input.text [ width (px 60), Font.size buttonFontSize ]
        { onChange = InputSeed
        , text = model.seedString
        , placeholder = Nothing
        , label = Input.labelLeft [] <| el [ Font.size buttonFontSize, moveDown 12 ] (text "seed ")
        }

stepButton : Element Msg
stepButton =
    row [ centerX ]
        [ Input.button buttonStyle
            { onPress = Just Step
            , label = el [ centerX, centerY ] (text "Step")
            }
        ]


runButton : Model -> Element Msg
runButton model =
    row [ centerX, width (px 80) ]
        [ Input.button (buttonStyle ++ [ activeBackgroundColor model ])
            { onPress = Just AdvanceAppState
            , label = el [ centerX, centerY, width (px 60) ] (text <| appStateAsString model.appState)
            }
        ]


activeBackgroundColor model =
    case model.appState of
        Running ->
            Background.color (Element.rgb 0.65 0 0)

        _ ->
            Background.color (Element.rgb 0 0 0)


resetButton : Element Msg
resetButton =
    row [ centerX ]
        [ Input.button buttonStyle
            { onPress = Just Reset
            , label = el [ centerX, centerY ] (text <| "Reset")
            }
        ]


appStateAsString : AppState -> String
appStateAsString appState =
    case appState of
        Ready ->
            "Ready"

        Running ->
            "Running"

        Paused ->
            "Paused"



--
-- STYLE
--


mainColumnStyle =
    [ centerX
    , centerY
    , Background.color (rgb255 240 240 240)
    , paddingXY 20 20
    ]


buttonStyle =
    [ Background.color (rgb255 40 40 40)
    , Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    , Font.size buttonFontSize
    ]



--
