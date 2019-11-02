module HeatEquationGL exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Browser
import CellGrid exposing (CellGrid(..))
import CellGrid.RenderWebGL
import Color exposing (Color)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import TemperatureField
import Time exposing (Posix)


config = {
     gridWidth = 100
   , gridDisplayWidth = 500
   , initialSeed = 3771
   , tickInterval = 30}






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
    , appState : AppState
    , beta : Float
    , betaString : String
    , heatMap : CellGrid Float
    }


type AppState
    = Ready
    | Running
    | Paused


type Msg
    = NoOp
    | InputBeta String
    | Step
    | Tick Posix
    | AdvanceAppState
    | Reset



-- | CellGrid CellGrid.Msg


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = "Test"
      , output = "Test"
      , counter = 0
      , appState = Ready
      , beta = 0.1
      , betaString = "0.1"
      , heatMap = initialTemperatureField
      }
    , Cmd.none
    )


initialTemperatureField : CellGrid Float
initialTemperatureField =
    let
        w =
            toFloat config.gridWidth

        c1 =
            floor <| 0.6 * w

        c2 =
            floor <| 0.2 * w

        r1 =
            0.2 * w

        r2 =
            0.1 * w
    in
    TemperatureField.randomHeatMap  {rows = config.gridWidth, columns = config.gridWidth}
        |> TemperatureField.spot ( c1, c1 ) r1 1.0
        |> TemperatureField.spot ( c2, c2 ) r1 0.0
        |> TemperatureField.spot ( c2, c2 ) r2 1.0


subscriptions model =
    Time.every config.tickInterval Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputBeta str ->
            case String.toFloat str of
                Nothing ->
                    ( { model | betaString = str }, Cmd.none )

                Just beta_ ->
                    ( { model | betaString = str, beta = beta_ }, Cmd.none )

        Step ->
            ( { model | counter = model.counter + 1, heatMap = TemperatureField.updateCells model.beta model.heatMap }, Cmd.none )

        Tick t ->
            case model.appState == Running of
                True ->
                    ( { model | counter = model.counter + 1, heatMap = TemperatureField.updateCells model.beta model.heatMap }, Cmd.none )

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
            ( { model | counter = 0, appState = Ready, heatMap = initialTemperatureField }, Cmd.none )



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [ Background.color <| Element.rgb 0 0 0 ] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ centerX, spacing 20 ]
            [ title "Diffusion of Heat"
            , el [] (CellGrid.RenderWebGL.asHtml {width = config.gridDisplayWidth, height = config.gridDisplayWidth} colorMap model.heatMap  |> Element.html)
            , row [ spacing 18 ]
                [ resetButton
                , runButton model
                , row [ spacing 8 ] [ stepButton, counterDisplay model ]
                , inputBeta model
                ]
            , el [ Font.size 14, centerX, Font.color <| gray 0.5 ] (text "WebGL version, grid = 100x100; Run with 0 < beta < 1.0")
            , Element.newTabLink [ Font.size 14, centerX, Font.color <| Element.rgb 0.4 0.4 1 ]
                { url = "https://github.com/jxxcarlson/elm-cell-grid/tree/master/examples/HeatEquation"
                , label = el [] (text "Code on GitHub")
                }
            ]
        ]


colorMap : Float -> Color.Color
colorMap t =
    Color.rgb t 0 0


gray g =
    Element.rgb g g g


counterDisplay : Model -> Element Msg
counterDisplay model =
    el [ Font.size 18, width (px 30), Font.color <| Element.rgb 1 1 1 ] (text <| String.fromInt model.counter)


title : String -> Element msg
title str =
    row [ centerX, Font.bold, Font.color <| gray 0.7 ] [ text str ]


outputDisplay : Model -> Element msg
outputDisplay model =
    row [ centerX ]
        [ text model.output ]


buttonFontSize =
    16


inputBeta : Model -> Element Msg
inputBeta model =
    Input.text [ width (px 60), height (px 30), Font.size buttonFontSize, Background.color (gray 0.8) ]
        { onChange = InputBeta
        , text = model.betaString
        , placeholder = Nothing
        , label = Input.labelLeft [] <| el [ Font.color <| Element.rgb 0.8 0.8 0.8, Font.size buttonFontSize, moveDown 10, moveLeft 5 ] (text "beta ")
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
            "Run"

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
    , Background.color (rgb255 0 0 0)
    , paddingXY 20 20
    ]


buttonStyle =
    [ Background.color (rgb255 40 40 40)
    , Font.color (gray 0.7)
    , paddingXY 15 8
    , Font.size buttonFontSize
    ]



--
