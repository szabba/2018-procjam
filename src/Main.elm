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
    }


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
        , H.ul [] <| List.map (H.li [] << List.singleton << viewSample) <| samples
        ]
    }


changeWantedDecoder : Decoder Msg
changeWantedDecoder =
    HE.targetValue
        |> Decode.map String.toInt
        |> Decode.map (Maybe.map CountWantedChanged)
        |> Decode.map (Maybe.withDefault NoOp)


viewSample : Sample -> Html msg
viewSample { skinTone } =
    H.span [ HA.style "color" <| skinToneToString skinTone ]
        [ H.text "skinTone" ]


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
    Random.map Sample skinToneGenerator


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
