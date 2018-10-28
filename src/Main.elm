--   This Source Code Form is subject to the terms of the Mozilla Public
--   License, v. 2.0. If a copy of the MPL was not distributed with this
--   file, You can obtain one at http://mozilla.org/MPL/2.0/.


module Main exposing (main)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode exposing (Decoder)
import Random exposing (Generator)
import Svg as S exposing (Svg)
import Svg.Attributes as SA


main : Program {} Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { generation : Int
    , requestedGeneration : Int
    , countWanted : Int
    , samples : List Sample
    }


type alias Sample =
    { skinTone : SkinTone
    , eyes : Eyes
    , hat : Maybe Hat
    , farHandItem : Maybe RotatedItem
    , closeHandItem : Maybe RotatedItem
    }


type Hat
    = Bowler
    | Top


type alias RotatedItem =
    { item : Item
    , arcFraction : Float
    }


type Eyes
    = EyePair
    | FarEye
    | CloseEye


type Item
    = Flower
    | Dagger
    | Cane


type SkinTone
    = SkinTone String


skinToneToString : SkinTone -> String
skinToneToString (SkinTone skinTone) =
    skinTone


init : {} -> ( Model, Cmd Msg )
init _ =
    ( { generation = 0
      , requestedGeneration = 1
      , countWanted = 10
      , samples = []
      }
    , generateSamples
        { generation = 1
        , count = 10
        }
    )


view : Model -> Browser.Document Msg
view { generation, samples, countWanted } =
    { title = "PROCJAM 2018"
    , body =
        [ H.input
            [ HA.type_ "range"
            , HA.min "1"
            , HA.max "30"
            , HA.value <| String.fromInt <| clamp 1 30 <| countWanted
            , HE.on "change" changeWantedDecoder
            ]
            []
        , H.button [ HE.onClick RegenerationRequested ]
            [ H.text "REGENERATE" ]
        , H.div [] <| List.map (H.div [ HA.style "display" "inline-block" ] << List.singleton << viewSample) <| samples
        ]
    }


changeWantedDecoder : Decoder Msg
changeWantedDecoder =
    HE.targetValue
        |> Decode.map String.toInt
        |> Decode.map (Maybe.map CountWantedChanged)
        |> Decode.map (Maybe.withDefault NoOp)


viewSample : Sample -> Html msg
viewSample sample =
    S.svg
        [ SA.width "200"
        , SA.height "230"
        , SA.viewBox "-25 -30 200 230"
        , HA.style "border-color" "#000"
        , HA.style "border-width" "1px"
        , HA.style "border-style" "solid"
        ]
        [ S.g
            [ SA.fill <| skinToneToString sample.skinTone
            , SA.strokeWidth "2"
            , SA.stroke "#000"
            ]
            [ viewFarHand sample
            , offset ( 75, 100 ) [ viewBody sample ]
            , offset ( 75, 50 )
                [ viewHead
                , viewEyes sample.eyes
                ]
            , sample.hat
                |> Maybe.map viewHat
                |> Maybe.map List.singleton
                |> Maybe.map (offset ( 75, 25 ))
                |> Maybe.withDefault (S.text "")
            , viewCloseHand sample
            ]
        ]


offset ( dx, dy ) children =
    let
        transform =
            "translate(" ++ String.fromInt dx ++ ", " ++ String.fromInt dy ++ ")"
    in
    S.g [ SA.transform transform ] children


viewHead : Svg msg
viewHead =
    S.circle [ SA.cx "0", SA.cy "0", SA.r "30" ] []


viewEyes : Eyes -> Svg msg
viewEyes eyes =
    S.g []
        [ if [ FarEye, EyePair ] |> List.member eyes then
            farEye

          else
            S.text ""
        , if [ CloseEye, EyePair ] |> List.member eyes then
            closeEye

          else
            S.text ""
        ]


viewHat : Hat -> Svg msg
viewHat hat =
    case hat of
        Bowler ->
            S.g []
                [ S.path
                    [ SA.d "M -20 -10 a 20 20 0 0 1 40 0" ]
                    []
                , S.rect
                    [ SA.x "-20"
                    , SA.y "-10"
                    , SA.height "10"
                    , SA.width "40"
                    ]
                    []
                , S.line
                    [ SA.strokeWidth "4"
                    , SA.x1 "-25"
                    , SA.y1 "0"
                    , SA.x2 "25"
                    , SA.y2 "0"
                    ]
                    []
                ]

        Top ->
            S.g []
                [ S.rect
                    [ SA.x "-20"
                    , SA.y "-50"
                    , SA.height "40"
                    , SA.width "40"
                    ]
                    []
                , S.rect
                    [ SA.x "-20"
                    , SA.y "-10"
                    , SA.height "10"
                    , SA.width "40"
                    ]
                    []
                , S.line
                    [ SA.strokeWidth "4"
                    , SA.x1 "-25"
                    , SA.y1 "0"
                    , SA.x2 "25"
                    , SA.y2 "0"
                    ]
                    []
                ]


farEye : Svg msg
farEye =
    S.ellipse [ SA.cx "-20", SA.cy "0", SA.rx "5", SA.ry "7" ] []


closeEye : Svg msg
closeEye =
    S.circle [ SA.cx "5", SA.cy "0", SA.r "8" ] []


viewBody : Sample -> Svg msg
viewBody { skinTone } =
    S.path
        [ SA.d "M -20 50 L -10 -10 L 30 -10 L 40 50 L 40 90 L 20 50 L 0 50 L -20 90 Z" ]
        []


viewCloseHand : Sample -> Svg msg
viewCloseHand sample =
    offset ( 120, 140 )
        [ sample.closeHandItem
            |> Maybe.map viewRotatedItem
            |> Maybe.withDefault (S.text "")
        , viewHand
        ]


viewFarHand : Sample -> Svg msg
viewFarHand sample =
    offset ( 40, 140 )
        [ viewHand
        , sample.farHandItem
            |> Maybe.map viewRotatedItem
            |> Maybe.withDefault (S.text "")
        ]


viewHand : Svg msg
viewHand =
    S.circle [ SA.r "10" ] []


viewRotatedItem : RotatedItem -> Svg msg
viewRotatedItem { item, arcFraction } =
    let
        angle =
            arcFraction * 360 |> round |> String.fromInt
    in
    S.g [ SA.transform <| "rotate(" ++ angle ++ ")" ]
        [ viewItem item ]


viewItem : Item -> Svg msg
viewItem item =
    case item of
        Flower ->
            flower

        Dagger ->
            dagger

        Cane ->
            cane


cane : Svg msg
cane =
    S.g [ SA.fillOpacity "0", SA.strokeWidth "4" ]
        [ S.path [ SA.d "M 10 10 a 10 10 0 0 0 -20 0 l 0 40" ] [] ]


dagger : Svg msg
dagger =
    S.g [ SA.transform "rotate(-45)" ]
        [ S.line [ SA.x1 "2", SA.y1 "0", SA.x2 "-12", SA.y2 "0" ] []
        , S.line [ SA.x1 "-12", SA.y1 "10", SA.x2 "-12", SA.y2 "-10" ] []
        , S.path [ SA.d "M -40 0 l 28 -5 l 0 10 Z" ] []
        ]


flower : Svg msg
flower =
    let
        flowerHead =
            [ offset ( 7, 0 ) [ S.circle [ SA.r "5" ] [] ]
            , offset ( 0, 7 ) [ S.circle [ SA.r "5" ] [] ]
            , offset ( -7, 0 ) [ S.circle [ SA.r "5" ] [] ]
            , offset ( 0, -7 ) [ S.circle [ SA.r "5" ] [] ]
            , S.circle [ SA.r "5" ] []
            ]
    in
    S.g []
        [ S.line [ SA.x1 "10", SA.y1 "-15", SA.x2 "-20", SA.y2 "30" ] []
        , offset ( -20, 30 ) flowerHead
        ]


type Msg
    = NoOp
    | CountWantedChanged Int
    | RegenerationRequested
    | Generated { generation : Int, samples : List Sample }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CountWantedChanged newCountWanted ->
            let
                newRequestedGeneration =
                    model.requestedGeneration + 1
            in
            ( { model
                | requestedGeneration = newRequestedGeneration
                , countWanted = newCountWanted
              }
            , generateSamples { generation = newRequestedGeneration, count = newCountWanted }
            )

        RegenerationRequested ->
            let
                newRequestedGeneration =
                    model.requestedGeneration + 1
            in
            ( { model | requestedGeneration = newRequestedGeneration }
            , generateSamples { generation = newRequestedGeneration, count = model.countWanted }
            )

        Generated { generation, samples } ->
            ( if generation == model.requestedGeneration then
                { model
                    | generation = generation
                    , samples = samples
                }

              else
                model
            , Cmd.none
            )


generateSamples : { generation : Int, count : Int } -> Cmd Msg
generateSamples { generation, count } =
    let
        withSamples samples =
            Generated
                { generation = generation
                , samples = samples
                }
    in
    sampleGenerator
        |> Random.list count
        |> Random.map withSamples
        |> Random.generate identity


sampleGenerator : Generator Sample
sampleGenerator =
    Random.map5 Sample
        skinToneGenerator
        eyesGenerator
        hatGenerator
        itemGenerator
        itemGenerator


eyesGenerator : Generator Eyes
eyesGenerator =
    Random.weighted ( 1, EyePair ) [ ( 0.1, FarEye ), ( 0.1, CloseEye ) ]


hatGenerator : Generator (Maybe Hat)
hatGenerator =
    [ Bowler, Top ]
        |> List.map Just
        |> Random.uniform Nothing


itemGenerator : Generator (Maybe RotatedItem)
itemGenerator =
    let
        spreadItemAcrossAngles ( item, angles ) =
            angles
                |> List.map (RotatedItem item)
    in
    [ ( Flower, [ 0, 0.25, 0.5, 0.75 ] )
    , ( Dagger, [ 0, 0.25, 0.5, 0.75 ] )
    , ( Cane, [ 0, 0.3 ] )
    ]
        |> List.concatMap spreadItemAcrossAngles
        |> List.map Just
        |> Random.uniform Nothing


skinToneGenerator : Generator SkinTone
skinToneGenerator =
    Random.uniform "#fff"
        [ "#C5C5C5"
        , "#2F2F2F"
        , "#FF6464"
        , "#FFAC64"
        , "#E6FF7F"
        , "#8BA031"
        , "#31A068"
        , "#4EFFA7"
        , "#36C1B1"
        , "#369DC1"
        , "#366FC1"
        , "#A7CBFF"
        , "#315891"
        , "#9E91FF"
        , "#C891FF"
        , "#FF91BD"
        , "#C72D6B"
        ]
        |> Random.map SkinTone
