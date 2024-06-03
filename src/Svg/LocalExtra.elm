module Svg.LocalExtra exposing (arc, circle, closedArc, ellipse, fillUniform, line, polygon, polyline, rotated, scaled, strokeUniform, strokeWidth, translated)

import Angle exposing (Angle)
import Arc2d
import Color exposing (Color)
import Length
import Parameter1d
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Quantity
import Svg.PathD as PathD
import Web.Dom
import Web.Svg


line :
    { start : { x : Float, y : Float }, end : { x : Float, y : Float } }
    -> List (Web.Dom.Modifier future)
    -> Web.Dom.Node future
line lineGeometry additionalModifiers =
    Web.Svg.element "line"
        ([ Web.Dom.attribute "x1" (lineGeometry.start.x |> String.fromFloat)
         , Web.Dom.attribute "y1" (lineGeometry.start.y |> String.fromFloat)
         , Web.Dom.attribute "x2" (lineGeometry.end.x |> String.fromFloat)
         , Web.Dom.attribute "y2" (lineGeometry.end.y |> String.fromFloat)
         ]
            ++ additionalModifiers
        )
        []


circle : { position : { x : Float, y : Float }, radius : Float } -> List (Web.Dom.Modifier future) -> Web.Dom.Node future
circle geometry additionalModifiers =
    Web.Svg.element "circle"
        ([ Web.Dom.attribute "cx" ((geometry.position.x |> String.fromFloat) ++ "px")
         , Web.Dom.attribute "cy" ((geometry.position.y |> String.fromFloat) ++ "px")
         , Web.Dom.attribute "r" ((geometry.radius |> String.fromFloat) ++ "px")
         ]
            ++ additionalModifiers
        )
        []


ellipse : { position : { x : Float, y : Float }, radiusX : Float, radiusY : Float } -> List (Web.Dom.Modifier future) -> Web.Dom.Node future
ellipse geometry additionalModifiers =
    Web.Svg.element "ellipse"
        ([ Web.Dom.attribute "cx" ((geometry.position.x |> String.fromFloat) ++ "px")
         , Web.Dom.attribute "cy" ((geometry.position.y |> String.fromFloat) ++ "px")
         , Web.Dom.attribute "rx" ((geometry.radiusX |> String.fromFloat) ++ "px")
         , Web.Dom.attribute "ry" ((geometry.radiusY |> String.fromFloat) ++ "px")
         ]
            ++ additionalModifiers
        )
        []


translated : { x : Float, y : Float } -> Web.Dom.Modifier future_
translated offset =
    Web.Dom.attribute "transform"
        ([ "translate("
         , offset.x |> String.fromFloat
         , ", "
         , offset.y |> String.fromFloat
         , ")"
         ]
            |> String.concat
        )


scaled : Float -> Web.Dom.Modifier future_
scaled scale =
    Web.Dom.attribute "transform"
        ([ "scale("
         , scale |> String.fromFloat
         , ", "
         , scale |> String.fromFloat
         , ")"
         ]
            |> String.concat
        )


rotated : { angle : Angle.Angle, center : { x : Float, y : Float } } -> Web.Dom.Modifier future_
rotated geometry =
    Web.Dom.attribute "transform"
        ([ "rotate("
         , geometry.angle |> Angle.inDegrees |> String.fromFloat
         , ", "
         , geometry.center.x |> String.fromFloat
         , ", "
         , geometry.center.y |> String.fromFloat
         , ")"
         ]
            |> String.concat
        )


fillUniform : Color -> Web.Dom.Modifier future_
fillUniform color =
    Web.Dom.attribute "fill" (color |> Color.toCssString)


strokeUniform : Color -> Web.Dom.Modifier future_
strokeUniform color =
    Web.Dom.attribute "stroke" (color |> Color.toCssString)


points : List { x : Float, y : Float } -> Web.Dom.Modifier future_
points =
    \points_ ->
        Web.Dom.attribute "points"
            ((case points_ of
                [ onlyElement ] ->
                    [ onlyElement, onlyElement ]

                notOnlyOne ->
                    notOnlyOne
             )
                |> List.map (\point -> [ point.x |> String.fromFloat, ",", point.y |> String.fromFloat ] |> String.concat)
                |> String.join " "
            )


polygon : Polygon2d Length.Meters Float -> List (Web.Dom.Modifier future) -> Web.Dom.Node future
polygon polygonGeometry additionalModifiers =
    Web.Svg.element "path"
        (Web.Dom.attribute "d"
            (PathD.pathD
                (case (polygonGeometry |> Polygon2d.outerLoop) :: (polygonGeometry |> Polygon2d.innerLoops) |> List.concat of
                    [] ->
                        []

                    startPoint :: nextPoints ->
                        PathD.M (startPoint |> Point2d.toTuple Length.inMeters)
                            :: (nextPoints
                                    |> List.map
                                        (\inBetweenPoint ->
                                            PathD.L (inBetweenPoint |> Point2d.toTuple Length.inMeters)
                                        )
                               )
                            ++ [ PathD.Z ]
                )
            )
            :: additionalModifiers
        )
        []


strokeWidth : Float -> Web.Dom.Modifier future_
strokeWidth pixels =
    Web.Dom.attribute "stroke-width" ((pixels |> String.fromFloat) ++ "px")


polyline : List { x : Float, y : Float } -> List (Web.Dom.Modifier future) -> Web.Dom.Node future
polyline points_ additionalModifiers =
    Web.Svg.element "polyline"
        (points points_
            :: additionalModifiers
        )
        []


arc : Arc2d.Arc2d Length.Meters coordinates -> List (Web.Dom.Modifier future_) -> Web.Dom.Node future_
arc arcGeometry modifiers =
    let
        maxSegmentAngle : Angle
        maxSegmentAngle =
            Angle.turns (1 / 3)

        numSegments : Int
        numSegments =
            1 + floor (abs (Quantity.ratio (arcGeometry |> Arc2d.sweptAngle) maxSegmentAngle))

        arcSegment : Float -> PathD.Segment
        arcSegment parameterValue =
            PathD.A
                ( Arc2d.radius arcGeometry |> Length.inMeters
                , Arc2d.radius arcGeometry |> Length.inMeters
                )
                0
                False
                (arcGeometry |> Arc2d.sweptAngle |> Quantity.greaterThanOrEqualTo Quantity.zero)
                (Arc2d.pointOn arcGeometry parameterValue |> Point2d.toTuple Length.inMeters)
    in
    Web.Svg.element "path"
        (Web.Dom.attribute "d"
            (PathD.pathD
                (PathD.M (Arc2d.startPoint arcGeometry |> Point2d.toTuple Length.inMeters)
                    :: Parameter1d.trailing numSegments arcSegment
                )
            )
            :: modifiers
        )
        []


closedArc : Arc2d.Arc2d Length.Meters coordinates -> List (Web.Dom.Modifier future_) -> Web.Dom.Node future_
closedArc arcGeometry modifiers =
    let
        maxSegmentAngle : Angle
        maxSegmentAngle =
            Angle.turns (1 / 3)

        numSegments : Int
        numSegments =
            1 + floor (abs (Quantity.ratio (arcGeometry |> Arc2d.sweptAngle) maxSegmentAngle))

        arcSegment : Float -> PathD.Segment
        arcSegment parameterValue =
            PathD.A
                ( Arc2d.radius arcGeometry |> Length.inMeters
                , Arc2d.radius arcGeometry |> Length.inMeters
                )
                0
                False
                (arcGeometry |> Arc2d.sweptAngle |> Quantity.greaterThanOrEqualTo Quantity.zero)
                (Arc2d.pointOn arcGeometry parameterValue |> Point2d.toTuple Length.inMeters)
    in
    Web.Svg.element "path"
        (Web.Dom.attribute "d"
            (PathD.pathD
                (PathD.M (arcGeometry |> Arc2d.centerPoint |> Point2d.toTuple Length.inMeters)
                    :: PathD.L (Arc2d.startPoint arcGeometry |> Point2d.toTuple Length.inMeters)
                    :: Parameter1d.trailing numSegments arcSegment
                    ++ [ PathD.Z ]
                )
            )
            :: modifiers
        )
        []
