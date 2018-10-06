import Browser
import Html exposing (Html)

import Svg exposing (svg)
import Svg.Attributes as SvgAttr

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region

-- COLORs

white =
    Element.rgb 1 1 1

blue =
    Element.rgb 0 0 0.8


-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Matchup =
  { playerOne : Person
  , playerTwo : Person
  , winner : Maybe Person
  }

type alias Person =
  { name : String
  , playing : Bool
  }

type alias Model =
  { name : String
  , people : List Person
  , matchups: List Matchup
  }

init : Model
init =
  Model
    "Fortsatt ikke Ivar"
    [Person "Ivar" True, Person "Ikke Ivar" True]
    [ Matchup (Person "Ivar" True) (Person "Ikke Ivar" True) Nothing
    , Matchup (Person "Ivar" True) (Person "Ikke Ivar" True) Nothing
    ]


-- UPDATE

type Msg
  = Change String
  | Add
  | Generate

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change name ->
      { model | name = name }

    Add ->
      { model
        | people = Person model.name True :: model.people
        , name = ""
      }

    Generate ->
      model

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
        , placeholder = Just (Input.placeholder [] (text "Participant"))
        , text = model.name
        }
      , Input.button
        [ Background.color blue
        , Font.color white
        , paddingXY 32 16
        ]
        { onPress = Just Add
        , label = text "Add player"
        }
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
          , { header = text "Playing"
            , width = px 100
            , view =
                \person -> text (playingString person.playing)
            }
          ]
        }
      , Input.button
        [ Background.color blue
        , Font.color white
        , paddingXY 16 8
        ]
        { onPress = Just Generate
        , label = text "Generate matchups"
        }
      , Element.html (viewMatchups model.matchups)
      ]

viewMatchups : List Matchup -> Html Msg
viewMatchups matchups =
  let
    viewBox = SvgAttr.viewBox
    fontSize = SvgAttr.fontSize
  in
  svg
    [ viewBox "0 0 800 800",
      fontSize "20"
    ]
    (List.indexedMap viewMatchup matchups)

viewMatchup index matchup =
  let
    text_ = Svg.text_
    text = Svg.text
    g = Svg.g

    x = SvgAttr.x
    y = SvgAttr.y
    r = SvgAttr.r
    fill = SvgAttr.fill
    width = SvgAttr.width
    height = SvgAttr.height
    transform = SvgAttr.transform
    color = SvgAttr.color
  in
  g
    [transform ("translate(0, " ++ (String.fromInt (20 + 80 * index)) ++ ")")
    ]
    [ text_ [] [text matchup.playerOne.name]
    , text_ [y "20"] [text matchup.playerTwo.name]
    ]


playingString : Bool -> String
playingString playing =
  case playing of
    True ->
      "Yes"

    False ->
      "No"
