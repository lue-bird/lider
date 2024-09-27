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
import Polygon2d
import Polyline2d exposing (Polyline2d)
import Quantity
import Random.Pcg.Extended
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Serialize
import Svg.LocalExtra
import Svg.PathD as PathD
import Time
import Vector2d exposing (Vector2d)
import Web


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
                    Web.urlRequest
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
                    , Web.pushUrl (initialized |> stateToAppUrl)
                    , Web.navigationListen
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
            Web.randomUnsignedInt32s 4
                |> Web.interfaceFutureMap InitialRandomnessReceived
    , case state.flapAudio of
        Just (Ok flapAudioSource) ->
            state.flapTimes
                |> List.map
                    (\flapAudio ->
                        Web.audioFromSource flapAudioSource flapAudio.time
                            |> Web.audioSpeedScaleBy
                                (Web.audioParameterAt
                                    (2 ^ ((flapAudio.nthPickedApple |> Basics.toFloat) * 0.01))
                                )
                    )
                |> List.map Web.audioPlay
                |> Web.interfaceBatch

        _ ->
            Web.audioSourceLoad "flap.mp3"
                |> Web.interfaceFutureMap FlapAudioReceived
    , Web.timePeriodicallyListen (Duration.milliseconds 16)
        |> Web.interfaceFutureMap SimulationTick
    , [ Web.windowSizeRequest, Web.windowResizeListen ]
        |> Web.interfaceBatch
        |> Web.interfaceFutureMap WindowSizeReceived
    , Web.windowListenTo "keydown"
        |> Web.interfaceFutureMap
            (\event ->
                event
                    |> Json.Decode.decodeValue
                        (Json.Decode.field "key" Json.Decode.string)
                    |> KeyPressed
            )
    , [ Web.gamepadsRequest, Web.gamepadsChangeListen ]
        |> Web.interfaceBatch
        |> Web.interfaceFutureMap
            (\gamepads ->
                GamepadReceived
                    (gamepads |> Dict.foldr (\_ gamepad _ -> gamepad |> Just) Nothing)
            )
    , let
        worldUi : Web.DomNode state_
        worldUi =
            Web.svgElement "rect"
                [ Svg.LocalExtra.fillUniform Color.black
                , Web.domAttribute "width" "100%"
                , Web.domAttribute "height" "100%"
                ]
                []

        playerUi : Web.DomNode future_
        playerUi =
            -- TODO
            []
                |> Web.svgElement "g" []

        controlsUi : Web.DomNode state_
        controlsUi =
            Web.svgElement "text"
                [ Svg.LocalExtra.fillUniform (Color.rgb 0.3 0.7 0.5)
                , Web.domStyle "font-size" "3em"
                , Web.domAttribute "text-anchor" "middle"
                , Web.domAttribute "dominant-baseline" "middle"
                , Web.domAttribute "font-weight" "bolder"
                , Web.domAttribute "x" "50%"
                , Web.domAttribute "y" "8%"
                , Web.domAttribute "width" "50%"
                , Web.domAttribute "height" "50%"
                ]
                [ "arrow keys or left controller thumbstick"
                    |> Web.domText
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
      Web.domElement "div"
        [ Web.domStyle "background-color" (Color.rgb 0.05 0.05 0.05 |> Color.toCssString)
        , Web.domStyle "position" "fixed"
        , Web.domStyle "top" "0"
        , Web.domStyle "right" "0"
        , Web.domStyle "bottom" "0"
        , Web.domStyle "left" "0"
        ]
        [ Web.svgElement "svg"
            [ Web.domAttribute "viewBox" ([ "0 0 ", worldSize.width |> String.fromFloat, " ", worldSize.height |> String.fromFloat ] |> String.concat)
            , Web.domAttribute "width" ((worldSize.width |> String.fromFloat) ++ "px")
            , Web.domAttribute "height" ((worldSize.height |> String.fromFloat) ++ "px")
            , Web.domStyle "display" "block"
            , Web.domStyle "margin" "auto"
            ]
            [ worldUi
            , controlsUi
            , playerUi
            , state.dockShapeCompositions
                |> List.map dockShapeCompositionUi
                |> Web.svgElement "g"
                    [ Svg.LocalExtra.scaled (worldSize.width / worldSizeCells.x)
                    ]
                |> List.singleton
                |> Web.svgElement "g"
                    [ Svg.LocalExtra.translated
                        { x = (state.windowSize.width |> Basics.toFloat) / 2
                        , y = (state.windowSize.height |> Basics.toFloat) / 2
                        }
                    ]
            ]
        ]
        |> Web.domRender
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
                    (Random.Pcg.Extended.list 3 (dockShapeGeneratorWithSubCount 60)
                        |> Random.Pcg.Extended.map
                            (\dockShapeCompositions ->
                                dockShapeCompositions
                                    |> List.indexedMap
                                        (\i dockShapeComposition ->
                                            dockShapeComposition
                                                |> dockShapeCompositionTranslateBy
                                                    (Vector2d.fromMeters { x = -25 + 25 * i |> Basics.toFloat, y = 0 })
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


dockShapeCompositionUi : DockShapeComposition -> Web.DomNode state_
dockShapeCompositionUi =
    \dockShapeComposition ->
        dockShapeComposition.shapes
            |> List.map
                (\shape ->
                    Web.svgElement "path"
                        [ Web.domAttribute "d"
                            (PathD.pathD
                                (case shape.geometry of
                                    [] ->
                                        []

                                    pathSegment0 :: pathSegment1Up ->
                                        PathD.M (pathSegment0 |> shapePathSegmentStartPoint |> Point2d.toTuple Length.inMeters)
                                            :: ((pathSegment0 :: pathSegment1Up)
                                                    |> List.concatMap shapePathSegmentToPathD
                                               )
                                            ++ [ PathD.Z ]
                                )
                            )
                        , Svg.LocalExtra.fillUniform shape.color
                        ]
                        []
                )
            |> Web.svgElement "g" []


shapePathSegmentStartPoint : ShapePathSegment -> Point2d Length.Meters Float
shapePathSegmentStartPoint =
    \shapePathSegment ->
        case shapePathSegment of
            ShapePathSegmentLine line ->
                line |> LineSegment2d.startPoint

            ShapePathSegmentArc arc ->
                arc |> Arc2d.startPoint


shapePathSegmentToPathD : ShapePathSegment -> List PathD.Segment
shapePathSegmentToPathD =
    \shapePathSegment ->
        case shapePathSegment of
            ShapePathSegmentLine line ->
                [ PathD.L (line |> LineSegment2d.endPoint |> Point2d.toTuple Length.inMeters) ]

            ShapePathSegmentArc arc ->
                pathDArc arc


pathDArc : Arc2d.Arc2d Length.Meters coordinates -> List PathD.Segment
pathDArc arcGeometry =
    let
        maxSegmentAngle : Angle
        maxSegmentAngle =
            Angle.turns (1 / 3)

        numSegments : Int
        numSegments =
            1 + floor (abs (Quantity.ratio (arcGeometry |> Arc2d.sweptAngle) maxSegmentAngle))
    in
    Parameter1d.trailing numSegments
        (\parameterValue ->
            PathD.A
                ( Arc2d.radius arcGeometry |> Length.inMeters
                , Arc2d.radius arcGeometry |> Length.inMeters
                )
                0
                False
                (arcGeometry |> Arc2d.sweptAngle |> Quantity.greaterThanOrEqualTo Quantity.zero)
                (Arc2d.pointOn arcGeometry parameterValue |> Point2d.toTuple Length.inMeters)
        )


worldSizeCells : { x : Float, y : Float }
worldSizeCells =
    { x = 80, y = 45 }


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
            |> Maybe.andThen
                (\str ->
                    str |> Serialize.decodeFromString stateCodec |> Result.toMaybe
                )


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


type alias ShapeGeometry =
    List ShapePathSegment


type ShapePathSegment
    = ShapePathSegmentLine (LineSegment2d Length.Meters Float)
    | ShapePathSegmentArc (Arc2d Length.Meters Float)


shapeGeometriesOverlap : ShapeGeometry -> ShapeGeometry -> Maybe (Point2d Length.Meters Float)
shapeGeometriesOverlap a b =
    let
        bSegments : List (LineSegment2d Length.Meters Float)
        bSegments =
            b |> shapePathApproximate |> Polyline2d.segments
    in
    a
        |> shapePathApproximate
        |> Polyline2d.segments
        |> listFirstJustMap
            (\aSegment ->
                bSegments
                    |> listFirstJustMap
                        (\bSegment ->
                            LineSegment2d.intersectionPoint aSegment bSegment
                        )
            )


listFirstJustMap : (a -> Maybe b) -> (List a -> Maybe b)
listFirstJustMap elementToMaybeFound =
    \list ->
        case list of
            [] ->
                Nothing

            head :: tail ->
                case elementToMaybeFound head of
                    Just found ->
                        Just found

                    Nothing ->
                        listFirstJustMap elementToMaybeFound tail


shapePathApproximate : ShapeGeometry -> Polyline2d Length.Meters Float
shapePathApproximate =
    \pathSegments ->
        let
            points : List (Point2d Length.Meters Float)
            points =
                pathSegments
                    |> List.concatMap
                        (\segment ->
                            case segment of
                                ShapePathSegmentLine line ->
                                    line |> LineSegment2d.endPoint |> List.singleton

                                ShapePathSegmentArc arc ->
                                    arc
                                        |> Arc2d.approximate (Length.meters 0.4)
                                        |> Polyline2d.vertices
                                        |> List.drop 1
                        )
        in
        Polyline2d.fromVertices points


debugColorForSubCount : Int -> Color
debugColorForSubCount =
    \subCount ->
        Color.hsl
            (0.25 + (subCount |> Basics.toFloat) / 400)
            0.5
            0.5


shapeGeometryTranslateBy : Vector2d Length.Meters Float -> (ShapeGeometry -> ShapeGeometry)
shapeGeometryTranslateBy displacement =
    \shapeGeometry ->
        shapeGeometry |> List.map (\segment -> segment |> shapePathSegmentTranslateBy displacement)


shapePathSegmentTranslateBy : Vector2d Length.Meters Float -> (ShapePathSegment -> ShapePathSegment)
shapePathSegmentTranslateBy displacement =
    \segment ->
        case segment of
            ShapePathSegmentLine lineEndPoint ->
                lineEndPoint
                    |> LineSegment2d.translateBy displacement
                    |> ShapePathSegmentLine

            ShapePathSegmentArc arcGeometry ->
                arcGeometry
                    |> Arc2d.translateBy displacement
                    |> ShapePathSegmentArc


shapeGeometryRotateAround : Point2d Length.Meters Float -> Angle -> (ShapeGeometry -> ShapeGeometry)
shapeGeometryRotateAround pivotPoint angleToRotateBy =
    \shapeGeometry ->
        shapeGeometry |> List.map (\segment -> segment |> shapePathSegmentRotateAround pivotPoint angleToRotateBy)


shapePathSegmentRotateAround : Point2d Length.Meters Float -> Angle -> (ShapePathSegment -> ShapePathSegment)
shapePathSegmentRotateAround pivotPoint angleToRotateBy =
    \shapeGeometry ->
        case shapeGeometry of
            ShapePathSegmentLine lineEndPoint ->
                lineEndPoint
                    |> LineSegment2d.rotateAround pivotPoint angleToRotateBy
                    |> ShapePathSegmentLine

            ShapePathSegmentArc arcGeometry ->
                arcGeometry
                    |> Arc2d.rotateAround pivotPoint angleToRotateBy
                    |> ShapePathSegmentArc


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
        arcOutlineDockShapeSegmentRandomGenerator
        [ lineDockShapeSegmentRandomGenerator
        , halfDiscDockShapeSegmentRandomGenerator
        , thirdDiscDockShapeSegmentRandomGenerator
        , quarterDiscDockShapeSegmentRandomGenerator
        , equilateralPolygonDockShapeSegmentRandomGenerator
        ]


arcOutlineDockShapeSegmentRandomGenerator :
    Random.Pcg.Extended.Generator
        { shapeGeometry : List ShapePathSegment
        , docks : ( LineSegment2d Length.Meters Float, List (LineSegment2d Length.Meters Float) )
        }
arcOutlineDockShapeSegmentRandomGenerator =
    Random.Pcg.Extended.constant
        (\radius spanAngle ->
            let
                arcGeometryWithRadius : Length -> Arc2d.Arc2d Length.Meters Float
                arcGeometryWithRadius arcRadius =
                    Arc2d.with
                        { radius = arcRadius
                        , sweptAngle = spanAngle
                        , centerPoint = Point2d.origin
                        , startAngle = Angle.turns 0
                        }

                arcInnerGeometry : Arc2d.Arc2d Length.Meters Float
                arcInnerGeometry =
                    arcGeometryWithRadius (radius |> Quantity.minus (Length.meters 0.5))
                        |> Arc2d.reverse

                arcOuterGeometry : Arc2d.Arc2d Length.Meters Float
                arcOuterGeometry =
                    arcGeometryWithRadius (radius |> Quantity.plus (Length.meters 0.5))

                startLine : LineSegment2d Length.Meters Float
                startLine =
                    LineSegment2d.from
                        (arcInnerGeometry |> Arc2d.endPoint)
                        (arcOuterGeometry |> Arc2d.startPoint)

                endLine : LineSegment2d Length.Meters Float
                endLine =
                    LineSegment2d.from
                        (arcOuterGeometry |> Arc2d.endPoint)
                        (arcInnerGeometry |> Arc2d.startPoint)
            in
            { shapeGeometry =
                [ ShapePathSegmentArc arcOuterGeometry
                , ShapePathSegmentLine endLine
                , ShapePathSegmentArc arcInnerGeometry
                , ShapePathSegmentLine startLine
                ]
            , docks =
                ( startLine
                , [ endLine ]
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


lineSegment2dSubdivide : Int -> (LineSegment2d units coordinates -> List (LineSegment2d units coordinates))
lineSegment2dSubdivide subdivisionCount =
    \lineSegment ->
        let
            step : Float
            step =
                1 / (subdivisionCount |> Basics.toFloat)
        in
        Parameter1d.leading subdivisionCount
            (\startPercentage ->
                LineSegment2d.from (LineSegment2d.interpolate lineSegment startPercentage)
                    (LineSegment2d.interpolate lineSegment (startPercentage + step))
            )


quarterDiscDockShapeSegmentRandomGenerator :
    Random.Pcg.Extended.Generator
        { shapeGeometry : ShapeGeometry
        , docks : ListFilled (LineSegment2d Length.Meters Float)
        }
quarterDiscDockShapeSegmentRandomGenerator =
    discDockShapeSegmentRandomGeneratorWithSpanAngle (Angle.turns (1.0 / 4.0))


thirdDiscDockShapeSegmentRandomGenerator :
    Random.Pcg.Extended.Generator
        { shapeGeometry : ShapeGeometry
        , docks : ListFilled (LineSegment2d Length.Meters Float)
        }
thirdDiscDockShapeSegmentRandomGenerator =
    discDockShapeSegmentRandomGeneratorWithSpanAngle (Angle.turns (1.0 / 3.0))


halfDiscDockShapeSegmentRandomGenerator :
    Random.Pcg.Extended.Generator
        { shapeGeometry : ShapeGeometry
        , docks : ListFilled (LineSegment2d Length.Meters Float)
        }
halfDiscDockShapeSegmentRandomGenerator =
    discDockShapeSegmentRandomGeneratorWithSpanAngle (Angle.turns (1.0 / 2.0))


discDockShapeSegmentRandomGeneratorWithSpanAngle :
    Angle
    ->
        Random.Pcg.Extended.Generator
            { shapeGeometry : ShapeGeometry
            , docks : ListFilled (LineSegment2d Length.Meters Float)
            }
discDockShapeSegmentRandomGeneratorWithSpanAngle spanAngle =
    Random.Pcg.Extended.constant
        (\radiusInMeters ->
            let
                arcGeometry : Arc2d Length.Meters coordinates
                arcGeometry =
                    Arc2d.with
                        { radius = Length.meters (radiusInMeters |> Basics.toFloat)
                        , sweptAngle = spanAngle
                        , centerPoint = Point2d.origin
                        , startAngle = Angle.turns 0
                        }

                arcCenterToStart : List (LineSegment2d Length.Meters coordinates)
                arcCenterToStart =
                    lineSegment2dSubdivide radiusInMeters
                        (LineSegment2d.from
                            (arcGeometry |> Arc2d.centerPoint)
                            (arcGeometry |> Arc2d.startPoint)
                        )

                arcEndToCenter : List (LineSegment2d Length.Meters coordinates)
                arcEndToCenter =
                    lineSegment2dSubdivide radiusInMeters
                        (LineSegment2d.from
                            (arcGeometry |> Arc2d.endPoint)
                            (arcGeometry |> Arc2d.centerPoint)
                        )
            in
            { shapeGeometry =
                (arcCenterToStart |> List.map ShapePathSegmentLine)
                    ++ [ arcGeometry |> ShapePathSegmentArc ]
                    ++ (arcEndToCenter |> List.map ShapePathSegmentLine)
            , docks =
                case arcCenterToStart ++ arcEndToCenter of
                    head :: tail ->
                        ( head, tail )

                    -- dummy
                    [] ->
                        ( LineSegment2d.from Point2d.origin Point2d.origin, [] )
            }
        )
        |> Random.Pcg.Extended.andMap
            (Random.Pcg.Extended.int 1 2)


lineDockShapeSegmentRandomGenerator :
    Random.Pcg.Extended.Generator
        { shapeGeometry : ShapeGeometry
        , docks : ListFilled (LineSegment2d Length.Meters Float)
        }
lineDockShapeSegmentRandomGenerator =
    Random.Pcg.Extended.constant
        (\length ->
            let
                halfLengthInMeters : Float
                halfLengthInMeters =
                    (length |> Length.inMeters) / 2

                topLeft : Point2d Length.Meters coordinates
                topLeft =
                    Point2d.fromMeters { x = -0.5, y = halfLengthInMeters }

                topRight : Point2d Length.Meters coordinates
                topRight =
                    Point2d.fromMeters { x = 0.5, y = halfLengthInMeters }

                bottomRight : Point2d Length.Meters coordinates
                bottomRight =
                    Point2d.fromMeters { x = 0.5, y = -halfLengthInMeters }

                bottomLeft : Point2d Length.Meters coordinates
                bottomLeft =
                    Point2d.fromMeters { x = -0.5, y = -halfLengthInMeters }

                bottomSide : LineSegment2d Length.Meters coordinates
                bottomSide =
                    LineSegment2d.from bottomLeft bottomRight

                rightSide : LineSegment2d Length.Meters coordinates
                rightSide =
                    LineSegment2d.from bottomRight topRight

                topSide : LineSegment2d Length.Meters coordinates
                topSide =
                    LineSegment2d.from topRight topLeft

                leftSide : LineSegment2d Length.Meters coordinates
                leftSide =
                    LineSegment2d.from topLeft bottomLeft
            in
            { shapeGeometry =
                [ ShapePathSegmentLine bottomSide
                , ShapePathSegmentLine rightSide
                , ShapePathSegmentLine topSide
                , ShapePathSegmentLine leftSide
                ]
            , docks =
                ( bottomSide
                , [ topSide ]
                )
            }
        )
        |> Random.Pcg.Extended.andMap
            (Random.Pcg.Extended.map Length.meters
                (Random.Pcg.Extended.int 1 3
                    |> Random.Pcg.Extended.map Basics.toFloat
                )
            )


equilateralPolygonDockShapeSegmentRandomGenerator :
    Random.Pcg.Extended.Generator
        { shapeGeometry : ShapeGeometry
        , docks : ListFilled (LineSegment2d Length.Meters Float)
        }
equilateralPolygonDockShapeSegmentRandomGenerator =
    Random.Pcg.Extended.constant
        (\sideCount longWidthInMeters ->
            let
                polygonGeometry =
                    Polygon2d.regular
                        { centerPoint = Point2d.origin
                        , circumradius =
                            Length.meters
                                (1
                                    / (Basics.sin
                                        (Basics.turns
                                            (1 / (2 * (sideCount |> Basics.toFloat)))
                                        )
                                        * 2
                                      )
                                )
                        , numSides = sideCount
                        }

                sides =
                    (polygonGeometry |> Polygon2d.outerLoop) |> listConsecutiveMap LineSegment2d.from
            in
            { shapeGeometry =
                sides |> List.map ShapePathSegmentLine
            , docks =
                case sides of
                    side0 :: side1Up ->
                        ( side0, side1Up )

                    [] ->
                        -- dummy for polygon with <= 1 vertices
                        ( LineSegment2d.from Point2d.origin Point2d.origin
                        , []
                        )
            }
        )
        |> Random.Pcg.Extended.andMap
            (Random.Pcg.Extended.int 3 8)
        |> Random.Pcg.Extended.andMap
            (Random.Pcg.Extended.int 1 3)


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


listConsecutiveMap : (a -> a -> b) -> List a -> List b
listConsecutiveMap combineConsecutive =
    \list ->
        List.map2 combineConsecutive list (list |> List.drop 1)


type alias ListFilled a =
    ( a, List a )


listFilledMap : (a -> b) -> ListFilled a -> ListFilled b
listFilledMap elementChange =
    \( head, tail ) ->
        ( head |> elementChange, tail |> List.map elementChange )


listFilledAttachList : List a -> ListFilled a -> ListFilled a
listFilledAttachList elementsToPutAfterExisting =
    \( head, tail ) ->
        ( head, tail ++ elementsToPutAfterExisting )


listFilledHead : ListFilled a -> a
listFilledHead =
    \( head, _ ) -> head


listFilledTail : ListFilled a -> List a
listFilledTail =
    \( _, tail ) -> tail


listFilledToList : ListFilled a -> List a
listFilledToList =
    \( head, tail ) -> head :: tail
