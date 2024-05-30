port module Main exposing (main)

import Angle exposing (Angle)
import AppUrl exposing (AppUrl)
import Arc2d exposing (Arc2d)
import Axis2d
import Color exposing (Color)
import Dict exposing (Dict)
import Direction2d
import Duration exposing (Duration)
import Json.Decode
import Json.Encode
import Length exposing (Length)
import LineSegment2d exposing (LineSegment2d)
import Parameter1d
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Quantity
import Random.Pcg.Extended
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Serialize
import Set
import Svg.LocalExtra
import Svg.PathD as PathD
import Time
import Vector2d exposing (Vector2d)
import Web
import Web.Audio
import Web.Audio.Parameter
import Web.Dom
import Web.Gamepads
import Web.Navigation
import Web.Random
import Web.Svg
import Web.Time
import Web.Window


main : Web.Program State
main =
    Web.program programConfig


programConfig : Web.ProgramConfig State
programConfig =
    { initialState = WaitingForInitialUrl
    , interface =
        \stateChoice ->
            case stateChoice of
                WaitingForInitialUrl ->
                    Web.Navigation.urlRequest
                        |> Web.interfaceFutureMap
                            (\initialUrl ->
                                case initialUrl |> appUrlToState of
                                    Just decodedState ->
                                        decodedState |> Initialized

                                    Nothing ->
                                        initialInitializedState |> Initialized
                            )

                Initialized initialized ->
                    [ initialized |> initializedInterface
                    , Web.Navigation.pushUrl (initialized |> stateToAppUrl)
                    , Web.Navigation.movementListen
                        |> Web.interfaceFutureMap
                            (\newUrl ->
                                case newUrl |> appUrlToState of
                                    Nothing ->
                                        let
                                            _ =
                                                Debug.log "failed to decode AppUrl" newUrl
                                        in
                                        initialized

                                    Just newState ->
                                        { initialized
                                            | flapAudio = initialized.flapAudio
                                            , flapTimes = initialized.flapTimes
                                        }
                            )
                    ]
                        |> Web.interfaceBatch
                        |> Web.interfaceFutureMap Initialized
    , ports = { fromJs = fromJs, toJs = toJs }
    }


initialInitializedState : InitializedState
initialInitializedState =
    { windowSize = dummyWindowSize
    , randomness = Nothing
    , dockShapeCompositions = []
    , headDirection = Right
    , location = Point2d.fromMeters { x = 4, y = 5 }
    , lastSimulationTime = Time.millisToPosix 0
    , flapAudio = Nothing
    , flapTimes = []
    }


dummyWindowSize : { width : Int, height : Int }
dummyWindowSize =
    { width = 1920, height = 1080 }


initializedInterface : InitializedState -> Web.Interface InitializedState
initializedInterface state =
    [ case state.randomness of
        Just _ ->
            Web.interfaceNone

        Nothing ->
            Web.Random.unsignedInt32s 4
                |> Web.interfaceFutureMap InitialRandomnessReceived
    , case state.flapAudio of
        Just (Ok flapAudioSource) ->
            state.flapTimes
                |> List.map
                    (\flapAudio ->
                        Web.Audio.fromSource flapAudioSource flapAudio.time
                            |> Web.Audio.speedScaleBy
                                (Web.Audio.Parameter.at
                                    (2 ^ ((flapAudio.nthPickedApple |> Basics.toFloat) * 0.01))
                                )
                    )
                |> List.map Web.Audio.play
                |> Web.interfaceBatch

        _ ->
            Web.Audio.sourceLoad "flap.mp3"
                |> Web.interfaceFutureMap FlapAudioReceived
    , Web.Time.periodicallyListen (Duration.milliseconds 16)
        |> Web.interfaceFutureMap SimulationTick
    , [ Web.Window.sizeRequest, Web.Window.resizeListen ]
        |> Web.interfaceBatch
        |> Web.interfaceFutureMap WindowSizeReceived
    , Web.Window.listenTo "keydown"
        |> Web.interfaceFutureMap
            (\event ->
                event
                    |> Json.Decode.decodeValue
                        (Json.Decode.field "key" Json.Decode.string)
                    |> KeyPressed
            )
    , [ Web.Gamepads.request, Web.Gamepads.changeListen ]
        |> Web.interfaceBatch
        |> Web.interfaceFutureMap
            (\gamepads ->
                GamepadReceived
                    (gamepads |> Dict.foldr (\_ gamepad _ -> gamepad |> Just) Nothing)
            )
    , let
        worldUi : Web.Dom.Node state_
        worldUi =
            Web.Svg.element "rect"
                [ Svg.LocalExtra.fillUniform Color.black
                , Web.Dom.attribute "width" "100%"
                , Web.Dom.attribute "height" "100%"
                ]
                []

        playerUi : Web.Dom.Node future_
        playerUi =
            -- TODO
            []
                |> Web.Svg.element "g" []

        controlsUi : Web.Dom.Node state_
        controlsUi =
            Web.Svg.element "text"
                [ Svg.LocalExtra.fillUniform (Color.rgb 0.3 0.7 0.5)
                , Web.Dom.style "font-size" "3em"
                , Web.Dom.attribute "text-anchor" "middle"
                , Web.Dom.attribute "dominant-baseline" "middle"
                , Web.Dom.attribute "font-weight" "bolder"
                , Web.Dom.attribute "x" "50%"
                , Web.Dom.attribute "y" "8%"
                , Web.Dom.attribute "width" "50%"
                , Web.Dom.attribute "height" "50%"
                ]
                [ "arrow keys or left controller thumbstick"
                    |> Web.Dom.text
                ]

        worldSize : { width : Float, height : Float }
        worldSize =
            let
                ratioWidthToHeight : Float
                ratioWidthToHeight =
                    worldSizeCells.x / worldSizeCells.y
            in
            if (state.windowSize.width |> Basics.toFloat) < (state.windowSize.height |> Basics.toFloat) * ratioWidthToHeight then
                -- disproportional in height
                { width = state.windowSize.width |> Basics.toFloat
                , height = (state.windowSize.width |> Basics.toFloat) / ratioWidthToHeight
                }

            else
                -- might be disproportional in width
                { width = (state.windowSize.height |> Basics.toFloat) * ratioWidthToHeight
                , height = state.windowSize.height |> Basics.toFloat
                }
      in
      Web.Dom.element "div"
        [ Web.Dom.style "background-color" (Color.rgb 0.05 0.05 0.05 |> Color.toCssString)
        , Web.Dom.style "position" "fixed"
        , Web.Dom.style "top" "0"
        , Web.Dom.style "right" "0"
        , Web.Dom.style "bottom" "0"
        , Web.Dom.style "left" "0"
        ]
        [ Web.Svg.element "svg"
            [ Web.Dom.attribute "viewBox" ([ "0 0 ", worldSize.width |> String.fromFloat, " ", worldSize.height |> String.fromFloat ] |> String.concat)
            , Web.Dom.attribute "width" ((worldSize.width |> String.fromFloat) ++ "px")
            , Web.Dom.attribute "height" ((worldSize.height |> String.fromFloat) ++ "px")
            , Web.Dom.style "display" "block"
            , Web.Dom.style "margin" "auto"
            ]
            [ worldUi
            , controlsUi
            , playerUi
            , state.dockShapeCompositions
                |> List.map dockShapeCompositionUi
                |> Web.Svg.element "g"
                    [ Svg.LocalExtra.scaled (worldSize.width / worldSizeCells.x)
                    ]
                |> List.singleton
                |> Web.Svg.element "g"
                    [ Svg.LocalExtra.translated
                        { x = (state.windowSize.width |> Basics.toFloat) / 2
                        , y = (state.windowSize.height |> Basics.toFloat) / 2
                        }
                    ]
            ]
        ]
        |> Web.Dom.render
    ]
        |> Web.interfaceBatch
        |> Web.interfaceFutureMap
            (\event ->
                case event of
                    WindowSizeReceived windowSize ->
                        { state | windowSize = windowSize }

                    InitialRandomnessReceived initialRandomness ->
                        case initialRandomness of
                            [] ->
                                state

                            initialRandomnessInt0 :: initialRandomnessInt1Up ->
                                state |> stateWithInitialRandomness ( initialRandomnessInt0, initialRandomnessInt1Up )

                    SimulationTick newTime ->
                        let
                            durationToSimulate : Duration
                            durationToSimulate =
                                Duration.from state.lastSimulationTime newTime

                            headMovement : Vector2d (Quantity.Rate Length.Meters Duration.Seconds) Float
                            headMovement =
                                directionToXYOffset state.headDirection

                            newLocation : Point2d Length.Meters Float
                            newLocation =
                                state.location
                                    |> Point2d.translateBy
                                        (headMovement |> Vector2d.for durationToSimulate)
                        in
                        { state
                            | lastSimulationTime = newTime
                            , location = newLocation
                        }

                    KeyPressed (Err _) ->
                        state

                    KeyPressed (Ok key) ->
                        case directionFromKeyboardKey |> Dict.get key of
                            Nothing ->
                                state

                            Just snakeDirection ->
                                { state | headDirection = snakeDirection }

                    GamepadReceived Nothing ->
                        state

                    GamepadReceived (Just gamepad) ->
                        case gamepad.thumbstickLeft |> directionFromThumbstick of
                            Nothing ->
                                state

                            Just snakeDirection ->
                                { state | headDirection = snakeDirection }

                    FlapAudioReceived received ->
                        { state | flapAudio = received |> Just }
            )


stateWithInitialRandomness : ( Int, List Int ) -> (InitializedState -> InitializedState)
stateWithInitialRandomness ( initialRandomnessInt0, initialRandomnessInt1Up ) =
    \state ->
        let
            initialSeed : Random.Pcg.Extended.Seed
            initialSeed =
                Random.Pcg.Extended.initialSeed initialRandomnessInt0 initialRandomnessInt1Up

            ( generatedDockShapeCompositions, newSeed ) =
                Random.Pcg.Extended.step
                    (Random.Pcg.Extended.list 3 (dockShapeGeneratorWithSubCount 70)
                        |> Random.Pcg.Extended.map
                            (\dockShapeCompositions ->
                                dockShapeCompositions
                                    |> List.indexedMap
                                        (\i dockShapeComposition ->
                                            dockShapeComposition
                                                |> dockShapeCompositionTranslateBy
                                                    (Vector2d.fromMeters { x = -10 + 10 * i |> Basics.toFloat, y = 0 })
                                        )
                            )
                    )
                    initialSeed
        in
        { state
            | randomness =
                { initial = ( initialRandomnessInt0, initialRandomnessInt1Up )
                , seed = newSeed
                }
                    |> Just
            , dockShapeCompositions = generatedDockShapeCompositions
        }


dockShapeCompositionUi : DockShapeComposition -> Web.Dom.Node state_
dockShapeCompositionUi =
    \dockShapeComposition ->
        dockShapeComposition.shapes
            |> List.map
                (\shape ->
                    case shape.geometry of
                        ClosingRoundShapeGeometry arcGeometry ->
                            Svg.LocalExtra.arc arcGeometry
                                [ Svg.LocalExtra.fillUniform shape.color
                                ]

                        PolygonShapeGeometry polygonGeometry ->
                            Svg.LocalExtra.polygon
                                polygonGeometry
                                [ Svg.LocalExtra.fillUniform shape.color
                                ]

                        ArcShapeGeometry arcGeometry ->
                            Svg.LocalExtra.arc arcGeometry
                                [ Svg.LocalExtra.strokeUniform shape.color
                                , Svg.LocalExtra.strokeWidth 1
                                , Svg.LocalExtra.fillUniform colorInvisible
                                ]
                )
            |> Web.Svg.element "g" []


worldSizeCells : { x : Float, y : Float }
worldSizeCells =
    { x = 80, y = 45 }


colorInvisible : Color
colorInvisible =
    Color.rgba 0 0 0 0


directionFromKeyboardKey : Dict String MainDirection
directionFromKeyboardKey =
    Dict.fromList
        [ ( "w", Up )
        , ( "a", Left )
        , ( "s", Down )
        , ( "d", Right )
        , ( "ArrowUp", Up )
        , ( "ArrowDown", Down )
        , ( "ArrowLeft", Left )
        , ( "ArrowRight", Right )
        ]


directionFromThumbstick : { x : Float, y : Float } -> Maybe MainDirection
directionFromThumbstick =
    \thumbCoordinates ->
        if (thumbCoordinates.y |> abs) <= 0.3 && (thumbCoordinates.x |> abs) <= 0.3 then
            Nothing

        else
            (if (thumbCoordinates.y |> abs) > (thumbCoordinates.x |> abs) then
                if thumbCoordinates.y < 0 then
                    Up

                else
                    Down

             else if thumbCoordinates.x < 0 then
                Left

             else
                Right
            )
                |> Just


directionToXYOffset : MainDirection -> Vector2d (Quantity.Rate Length.Meters Duration.Seconds) Float
directionToXYOffset direction =
    case direction of
        Up ->
            Vector2d.fromMeters { x = 0, y = -1 } |> Vector2d.per Duration.second

        Down ->
            Vector2d.fromMeters { x = 0, y = 1 } |> Vector2d.per Duration.second

        Left ->
            Vector2d.fromMeters { x = -1, y = 0 } |> Vector2d.per Duration.second

        Right ->
            Vector2d.fromMeters { x = 1, y = 0 } |> Vector2d.per Duration.second


stateCodec : Serialize.Codec error_ InitializedState
stateCodec =
    Serialize.record
        (\maybeInitialRandomness ->
            case maybeInitialRandomness of
                Nothing ->
                    initialInitializedState

                Just initialRandomness ->
                    initialInitializedState |> stateWithInitialRandomness initialRandomness
        )
        |> Serialize.field (\s -> s.randomness |> Maybe.map .initial)
            (Serialize.maybe
                (Serialize.record
                    (\int0 int1Up -> ( int0, int1Up ))
                    |> Serialize.field (\( head, _ ) -> head) Serialize.int
                    |> Serialize.field (\( _, tail ) -> tail) (Serialize.list Serialize.int)
                    |> Serialize.finishRecord
                )
            )
        |> Serialize.finishRecord


stateToAppUrl : InitializedState -> AppUrl
stateToAppUrl =
    \state ->
        { path = []
        , queryParameters =
            Dict.singleton ""
                [ state |> Serialize.encodeToString stateCodec ]
        , fragment = Nothing
        }


appUrlToState : AppUrl -> Maybe InitializedState
appUrlToState =
    \appUrl ->
        appUrl.queryParameters
            |> Dict.get ""
            |> Maybe.andThen List.head
            |> Maybe.andThen (\str -> str |> Serialize.decodeFromString stateCodec |> Result.toMaybe)


type State
    = WaitingForInitialUrl
    | Initialized InitializedState


type alias InitializedState =
    RecordWithoutConstructorFunction
        { windowSize : { width : Int, height : Int }
        , randomness :
            Maybe
                { initial : ( Int, List Int )
                , seed : Random.Pcg.Extended.Seed
                }
        , dockShapeCompositions : List DockShapeComposition
        , headDirection : MainDirection
        , location : Point2d Length.Meters Float
        , lastSimulationTime : Time.Posix
        , flapAudio : Maybe (Result Web.AudioSourceLoadError Web.AudioSource)
        , flapTimes : List { time : Time.Posix, nthPickedApple : Int }
        }


type alias DockShapeComposition =
    { shapes :
        List
            { geometry : ShapeGeometry
            , color : Color
            }
    , docks : List (LineSegment2d Length.Meters Float)
    }


type ShapeGeometry
    = ArcShapeGeometry (Arc2d Length.Meters Float)
    | PolygonShapeGeometry (Polygon2d Length.Meters Float)
    | ClosingRoundShapeGeometry (Arc2d Length.Meters Float)


debugColorForSubCount : Int -> Color
debugColorForSubCount =
    \subCount ->
        Color.hsl
            ((subCount |> Basics.toFloat) / 100)
            1
            0.5


shapeGeometryTranslateBy : Vector2d Length.Meters Float -> (ShapeGeometry -> ShapeGeometry)
shapeGeometryTranslateBy displacement =
    \shapeGeometry ->
        case shapeGeometry of
            PolygonShapeGeometry lineSegmentGeometry ->
                lineSegmentGeometry
                    |> Polygon2d.translateBy displacement
                    |> PolygonShapeGeometry

            ArcShapeGeometry arcGeometry ->
                arcGeometry
                    |> Arc2d.translateBy displacement
                    |> ArcShapeGeometry

            ClosingRoundShapeGeometry arcGeometry ->
                arcGeometry
                    |> Arc2d.translateBy displacement
                    |> ClosingRoundShapeGeometry


shapeGeometryRotateAround : Point2d Length.Meters Float -> Angle -> (ShapeGeometry -> ShapeGeometry)
shapeGeometryRotateAround pivotPoint angleToRotateBy =
    \shapeGeometry ->
        case shapeGeometry of
            PolygonShapeGeometry polygonGeometry ->
                polygonGeometry
                    |> Polygon2d.rotateAround pivotPoint angleToRotateBy
                    |> PolygonShapeGeometry

            ArcShapeGeometry arcGeometry ->
                arcGeometry
                    |> Arc2d.rotateAround pivotPoint angleToRotateBy
                    |> ArcShapeGeometry

            ClosingRoundShapeGeometry arcGeometry ->
                arcGeometry
                    |> Arc2d.rotateAround pivotPoint angleToRotateBy
                    |> ClosingRoundShapeGeometry


dockShapeCompositionTranslateBy : Vector2d Length.Meters Float -> (DockShapeComposition -> DockShapeComposition)
dockShapeCompositionTranslateBy displacement =
    \dockShape ->
        { dockShape
            | shapes =
                dockShape.shapes
                    |> List.map
                        (\shape ->
                            { shape
                                | geometry =
                                    shape.geometry |> shapeGeometryTranslateBy displacement
                            }
                        )
        }


dockShapeCompositionRotateAround : Point2d Length.Meters Float -> Angle -> (DockShapeComposition -> DockShapeComposition)
dockShapeCompositionRotateAround pivotPoint angleToRotateBy =
    \dockShape ->
        { dockShape
            | shapes =
                dockShape.shapes
                    |> List.map
                        (\shape ->
                            { shape
                                | geometry =
                                    shape.geometry |> shapeGeometryRotateAround pivotPoint angleToRotateBy
                            }
                        )
        }


dockShapeGeneratorWithSubCount : Int -> Random.Pcg.Extended.Generator DockShapeComposition
dockShapeGeneratorWithSubCount subCount =
    if subCount <= 1 then
        dockShapeLeafRandomGenerator
            |> Random.Pcg.Extended.map
                (\leaf ->
                    { shapes = [ { geometry = leaf.shapeGeometry, color = debugColorForSubCount 1 } ]
                    , docks = leaf.docks |> listFilledToList
                    }
                )

    else
        dockShapeGeneratorWithSubCount (subCount - 1)
            |> Random.Pcg.Extended.andThen
                (\soFar ->
                    case soFar.docks of
                        [] ->
                            Random.Pcg.Extended.constant soFar

                        soFarDock0 :: soFarDock1Up ->
                            dockShapeLeafRandomGenerator
                                |> Random.Pcg.Extended.andThen
                                    (\toCombineWith ->
                                        Random.Pcg.Extended.constant
                                            (\soFarDock toCombineWithDock ->
                                                let
                                                    soFarDockAngle : Angle
                                                    soFarDockAngle =
                                                        case soFarDock |> LineSegment2d.direction of
                                                            Just dockDirection ->
                                                                dockDirection |> Direction2d.toAngle

                                                            Nothing ->
                                                                Angle.turns 0

                                                    toCombineWithDockAngle : Angle
                                                    toCombineWithDockAngle =
                                                        case toCombineWithDock |> LineSegment2d.direction of
                                                            Just dockDirection ->
                                                                dockDirection |> Direction2d.toAngle

                                                            Nothing ->
                                                                Angle.turns 0

                                                    toCombineWithDisplacement : Vector2d Length.Meters Float
                                                    toCombineWithDisplacement =
                                                        soFarDock
                                                            |> LineSegment2d.midpoint
                                                            |> point2dToVector
                                                            |> Vector2d.plus
                                                                (toCombineWithDock
                                                                    |> LineSegment2d.midpoint
                                                                    |> point2dToVector
                                                                    |> Vector2d.scaleBy -1
                                                                )

                                                    toCombineWithRotation : Angle
                                                    toCombineWithRotation =
                                                        -- point in the opposite direction
                                                        soFarDockAngle
                                                            |> Quantity.plus (Angle.turns 0.5)
                                                            |> Quantity.minus toCombineWithDockAngle
                                                            |> Angle.normalize
                                                in
                                                { shapes =
                                                    { geometry =
                                                        toCombineWith.shapeGeometry
                                                            |> shapeGeometryTranslateBy toCombineWithDisplacement
                                                            |> shapeGeometryRotateAround
                                                                (soFarDock |> LineSegment2d.midpoint)
                                                                toCombineWithRotation
                                                    , color = debugColorForSubCount subCount
                                                    }
                                                        :: soFar.shapes
                                                , docks =
                                                    (toCombineWith.docks
                                                        |> listFilledToList
                                                        |> List.filter (\dock -> dock /= toCombineWithDock)
                                                        |> List.map
                                                            (\dock ->
                                                                dock
                                                                    |> LineSegment2d.translateBy toCombineWithDisplacement
                                                                    |> LineSegment2d.rotateAround
                                                                        (soFarDock |> LineSegment2d.midpoint)
                                                                        toCombineWithRotation
                                                            )
                                                    )
                                                        ++ ((soFarDock0 :: soFarDock1Up)
                                                                |> List.filter (\dock -> dock /= soFarDock)
                                                           )
                                                }
                                            )
                                            |> Random.Pcg.Extended.andMap
                                                (Random.Pcg.Extended.choices
                                                    (Random.Pcg.Extended.constant soFarDock0)
                                                    (soFarDock1Up |> List.map Random.Pcg.Extended.constant)
                                                )
                                            |> Random.Pcg.Extended.andMap
                                                (Random.Pcg.Extended.choices
                                                    (Random.Pcg.Extended.constant (toCombineWith.docks |> listFilledHead))
                                                    (toCombineWith.docks
                                                        |> listFilledTail
                                                        |> List.map Random.Pcg.Extended.constant
                                                    )
                                                )
                                    )
                )


dockShapeLeafRandomGenerator :
    Random.Pcg.Extended.Generator
        { shapeGeometry : ShapeGeometry
        , docks : ListFilled (LineSegment2d Length.Meters Float)
        }
dockShapeLeafRandomGenerator =
    Random.Pcg.Extended.choices
        (Random.Pcg.Extended.constant
            (\radius spanAngle ->
                let
                    arcGeometry : Arc2d Length.Meters Float
                    arcGeometry =
                        Arc2d.with
                            { radius = radius
                            , sweptAngle = spanAngle
                            , centerPoint = Point2d.origin
                            , startAngle = Angle.turns 0
                            }

                    arcInnerGeometry : Arc2d.Arc2d Length.Meters Float
                    arcInnerGeometry =
                        arcGeometry
                            |> arcRadiusAlter (\r -> r |> Quantity.minus (Length.meters 0.5))

                    arcOuterGeometry : Arc2d.Arc2d Length.Meters Float
                    arcOuterGeometry =
                        arcGeometry
                            |> arcRadiusAlter (\r -> r |> Quantity.plus (Length.meters 0.5))
                in
                { shapeGeometry = arcGeometry |> ArcShapeGeometry
                , docks =
                    ( LineSegment2d.from
                        (arcInnerGeometry |> Arc2d.startPoint)
                        (arcOuterGeometry |> Arc2d.startPoint)
                    , [ LineSegment2d.from
                            (arcOuterGeometry |> Arc2d.endPoint)
                            (arcInnerGeometry |> Arc2d.endPoint)
                      ]
                    )
                }
            )
            |> Random.Pcg.Extended.andMap
                (Random.Pcg.Extended.map Length.meters
                    (Random.Pcg.Extended.int 1 5
                        |> Random.Pcg.Extended.map Basics.toFloat
                    )
                )
            |> Random.Pcg.Extended.andMap
                (Random.Pcg.Extended.map Angle.turns
                    (Random.Pcg.Extended.float 0.1 0.25)
                )
        )
        [ lineDockShapeSegmentRandomGenerator
        , closingRoundDockShapeSegmentRandomGenerator
        ]


closingRoundDockShapeSegmentRandomGenerator :
    Random.Pcg.Extended.Generator
        { shapeGeometry : ShapeGeometry
        , docks : ListFilled (LineSegment2d Length.Meters Float)
        }
closingRoundDockShapeSegmentRandomGenerator =
    Random.Pcg.Extended.constant
        (\dist ->
            let
                arcGeometry : Arc2d Length.Meters Float
                arcGeometry =
                    Arc2d.with
                        { radius = Length.meters radiusInMeters
                        , sweptAngle = Angle.turns 0.5
                        , centerPoint = Point2d.origin
                        , startAngle = Angle.turns 0
                        }

                radiusInMeters : Float
                radiusInMeters =
                    (dist |> Basics.toFloat) / 2
            in
            { shapeGeometry = arcGeometry |> ClosingRoundShapeGeometry
            , docks =
                ( 1, List.range 2 dist )
                    |> listFilledMap
                        (\end ->
                            LineSegment2d.along Axis2d.x
                                (Length.meters (-radiusInMeters + (end - 1 |> Basics.toFloat)))
                                (Length.meters (-radiusInMeters + (end |> Basics.toFloat)))
                        )
            }
        )
        |> Random.Pcg.Extended.andMap
            (Random.Pcg.Extended.int 1 3)


lineDockShapeSegmentRandomGenerator :
    Random.Pcg.Extended.Generator
        { shapeGeometry : ShapeGeometry
        , docks : ListFilled (LineSegment2d Length.Meters Float)
        }
lineDockShapeSegmentRandomGenerator =
    Random.Pcg.Extended.constant
        (\radius ->
            let
                halfRadiusInMeters : Float
                halfRadiusInMeters =
                    (radius |> Length.inMeters) / 2

                topLeft : Point2d Length.Meters coordinates
                topLeft =
                    Point2d.fromMeters { x = -0.5, y = halfRadiusInMeters }

                topRight : Point2d Length.Meters coordinates
                topRight =
                    Point2d.fromMeters { x = 0.5, y = halfRadiusInMeters }

                bottomRight : Point2d Length.Meters coordinates
                bottomRight =
                    Point2d.fromMeters { x = 0.5, y = -halfRadiusInMeters }

                bottomLeft : Point2d Length.Meters coordinates
                bottomLeft =
                    Point2d.fromMeters { x = -0.5, y = -halfRadiusInMeters }
            in
            { shapeGeometry =
                Polygon2d.singleLoop [ bottomLeft, bottomRight, topRight, topLeft ]
                    |> PolygonShapeGeometry
            , docks =
                ( LineSegment2d.from bottomLeft bottomRight
                , [ LineSegment2d.from topRight topLeft
                  ]
                )
            }
        )
        |> Random.Pcg.Extended.andMap
            (Random.Pcg.Extended.map Length.meters
                (Random.Pcg.Extended.int 1 3
                    |> Random.Pcg.Extended.map Basics.toFloat
                )
            )


type MainDirection
    = Up
    | Right
    | Down
    | Left


type InitializedEvent
    = KeyPressed (Result Json.Decode.Error String)
    | GamepadReceived (Maybe Web.Gamepad)
    | SimulationTick Time.Posix
    | WindowSizeReceived { width : Int, height : Int }
    | InitialRandomnessReceived (List Int)
    | FlapAudioReceived (Result Web.AudioSourceLoadError Web.AudioSource)


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event


point2dToVector : Point2d Length.Meters coordinates -> Vector2d Length.Meters coordinated
point2dToVector =
    \point2d -> point2d |> Point2d.toMeters |> Vector2d.fromMeters


arcRadiusAlter : (Length -> Length) -> Arc2d Length.Meters coordinates -> Arc2d Length.Meters coordinates
arcRadiusAlter radiusChange =
    \arc ->
        arc
            |> Arc2d.scaleAbout (arc |> Arc2d.centerPoint)
                ((arc |> Arc2d.radius |> radiusChange |> Length.inMeters)
                    / (arc |> Arc2d.radius |> Length.inMeters)
                )


type alias ListFilled a =
    ( a, List a )


listFilledMap : (a -> b) -> ListFilled a -> ListFilled b
listFilledMap elementChange =
    \( head, tail ) ->
        ( head |> elementChange, tail |> List.map elementChange )


listFilledHead : ListFilled a -> a
listFilledHead =
    \( head, _ ) -> head


listFilledTail : ListFilled a -> List a
listFilledTail =
    \( _, tail ) -> tail


listFilledToList : ListFilled a -> List a
listFilledToList =
    \( head, tail ) -> head :: tail
