port module Main exposing (..)

import Browser
import Html exposing (Html)
import Set

import Svg exposing (svg)
import Svg.Attributes as SvgAttr

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region

import Json.Encode as E
import Json.Decode as Decode exposing (Decoder, Value, decodeValue)

import Api


-- COLORs

white =
    Element.rgb 1 1 1

blue =
    Element.rgb 0 0 0.8


-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- MODEL

type alias Matchup =
  { id : String
  , round : Int
  , playerOne : Maybe Int
  , playerTwo : Maybe Int
  , winner : Maybe Int
  }

type alias Person =
  { id : Int
  , name : String
  }

type alias Model =
  { name : String
  , people : List Person
  , matchups: List Matchup
  }

init : () -> (Model, Cmd Msg)
init _ =
  -- https://scelto.no/ansatte/
  -- copy([].map.call($$('.mugshot .sc-link'), (el,idx) => `, Person ${idx + 1} "${el.text}"`).join('\n'))
  ( Model
      ""
      [ Person 1 "Lars Olav Torvik"
      , Person 2 "Terje Lønøy"
      , Person 3 "Fredrik Svensen"
      , Person 4 "Seán Erik Scully"
      , Person 5 "Vetle Valebjørg"
      , Person 6 "Lars Fredrik Lunde"
      , Person 7 "Fredrik Bjørnøy"
      , Person 8 "Ken Gullaksen"
      , Person 9 "Bjarte Tynning"
      , Person 10 "Ivar Nilsen"
      , Person 11 "Jan Erik Svendsen"
      , Person 12 "Richard Rennemo"
      , Person 13 "Erik Salhus"
      , Person 14 "Gustav Bilben"
      , Person 15 "Erlend Nilsen"
      , Person 16 "Ole-André Riga-Johansen"
      , Person 17 "Marius Aune Gravdal"
      , Person 18 "Ole Tommy Lid-Strand"
      , Person 19 "Håken Stark"
      , Person 20 "Herman Crawfurd Svensen"
      , Person 21 "Tor Eric Sandvik"
      ]
      []
    , Cmd.none
  )


-- UPDATE

type Msg
  = Change String
  | Add
  | Generate
  | ReceiveMatchups (List Matchup)
  | Winner String Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change name ->
      ( { model | name = name }
        , Cmd.none
      )

    Add ->
      ( { model
          | people = Person (List.length model.people) model.name :: model.people
          , name = ""
        }
        , Cmd.none
      )

    Generate ->
      ( model
      , Api.createMatchups model.people
      )

    ReceiveMatchups value ->
      ( { model | matchups = value }
      , Cmd.none
      )

    Winner matchup winner ->
      ( model
      , Api.scoreMatchup matchup winner
      )


--SUBSCRIPTIONS

matchupsDecoder : Decoder (List Matchup)
matchupsDecoder =
  Decode.list
    (Decode.map5
      Matchup
      -- Maybe map to simpler structure in index.js
        (Decode.at ["id"] Decode.string)
        (Decode.at ["round"] Decode.int)
        (Decode.maybe (Decode.at ["playerOne"] Decode.int))
        (Decode.maybe (Decode.at ["playerTwo"] Decode.int))
        (Decode.maybe (Decode.at ["winner"] Decode.int))
    )

decodeMatchups : Value -> Msg
decodeMatchups value
  = case decodeValue matchupsDecoder value of
      Ok result ->
        ReceiveMatchups result

      Err err ->
        ReceiveMatchups []

subscriptions : Model -> Sub Msg
subscriptions _ =
  Api.onMatchupsUpdated decodeMatchups


-- VIEW

view : Model -> Html Msg
view model =
  Element.layout
    [ Font.size 20
    ]
  <|
    Element.column [ width (px 800), height shrink, alignTop, centerX, spacing 36, padding 10 ]
      [ el
        [ Region.heading 1
        , alignLeft
        , Font.size 36
        ]
        (text "Tournament")
      , Input.text
        [
        ]
        { label = Input.labelAbove [ Font.size 14 ] (text "Name")
        , onChange = \new -> Change new
        , placeholder = Just (Input.placeholder [] (text "Player"))
        , text = model.name
        }
      , Element.row [ width (px 800), alignLeft, spacing 15 ]
        [ Input.button
          [ Background.color blue
          , Font.color white
          , paddingXY 32 16
          ]
          { onPress = Just Add
          , label = text "Add player"
          }
        , Input.button
          [ Background.color blue
          , Font.color white
          , paddingXY 32 16
          ]
          { onPress = Just Generate
          , label = text "Generate matchups"
          }
        ]
      , el
        [ Region.heading 2
        , alignLeft
        , Font.size 24
        ]
        (text "Players")
      , Element.table
        [ spacing 15
        ]
        { data = model.people
        , columns =
          [ { header = text "Name"
            , width = px 300
            , view =
                \person -> text person.name
            }
          ]
        }
      , Element.row []
        ( List.map (\r -> viewRound r model.people) (getRounds model.matchups) )
      ]

getRounds : List Matchup -> List (List Matchup)
getRounds matchups =
  let
    rounds = Set.fromList (List.map (\m -> m.round) matchups)
  in
    List.map (\r -> List.filter (\m -> m.round == r) matchups) (Set.toList rounds)

viewRound : List Matchup -> List Person -> Element Msg
viewRound matches people =
  Element.column [ width (px 400), spacing 20]
    ( List.map (\m -> viewMatchup m people) matches )

viewMatchup : Matchup -> List Person -> Element Msg
viewMatchup matchup people =
  case (matchup.playerOne, matchup.playerTwo) of
    (Nothing, Nothing) ->
      Element.column [ height (px 50)]
      []

    (Just playerOne, Nothing) ->
      Element.column [ paddingXY 23 0, height (px 50)]
      [ text (findPlayerName playerOne people)
      ]

    (Nothing, Just playerTwo) ->
      Element.column [ paddingXY 23 0, height (px 50)]
      [ text (findPlayerName playerTwo people)
      ]

    (Just playerOne, Just playerTwo) ->
      if (playerTwo == -1 || playerOne == -1) then
        Element.column [ paddingXY 23 0, height (px 50)]
          [ text (findPlayerName playerOne people)
          , text (findPlayerName playerTwo people)
          ]
      else
        Element.column [ height (px 50)]
        [ Input.radio
          [ spacing 5]
          { selected = matchup.winner
          , onChange = \winner -> Winner matchup.id winner
          , label = Input.labelAbove
            [ Font.size 14
            , padding 0
            ]
            (text "")
          , options =
              [ Input.option
                  playerOne
                  (text (findPlayerName playerOne people))
              , Input.option
                  playerTwo
                  (text (findPlayerName playerTwo people))
              ]
          }
        ]

findPlayerName : Int -> List Person -> String
findPlayerName id people =
    case people |> List.filter (\p -> p.id == id) |> List.head of
      Just person ->
        person.name

      Nothing ->
        ""


playingString : Bool -> String
playingString playing =
  case playing of
    True ->
      "Yes"

    False ->
      "No"
