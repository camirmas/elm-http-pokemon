import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, int, string, at, object4)
import Task

main =
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

-- MODEL

type alias Pokemon =
  { name : String
  , height : Int
  , weight : Int
  , image : String
  }


type alias Model =
  { pokemon : Pokemon
  , search : String
  }

initPokemon : Pokemon
initPokemon =
  Pokemon "" 0 0 ""

init : (Model, Cmd Msg)
init =
  (Model initPokemon "", getPokemon "starmie")

-- UPDATE

type Msg
    = GetPokemon
    | UpdateSearch String
    | FetchSuccess Pokemon
    | FetchFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetPokemon ->
      (model, getPokemon model.search)

    UpdateSearch str ->
      (Model model.pokemon str, Cmd.none)

    FetchSuccess pokemon ->
      (Model pokemon "", Cmd.none)

    FetchFail _ ->
      (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  if model.pokemon.name /= "" then
    div []
      [ h2 [] [text model.pokemon.name]
      , img [src model.pokemon.image] []
      , ul []
        [ li [] [text ("Height: " ++ toString(model.pokemon.height))]
        , li [] [text ("Weight: " ++ toString(model.pokemon.weight))]
        ]
      , input [ placeholder "Get new pokemon"
              , value model.search
              , onInput UpdateSearch
              ] []
      , button [onClick GetPokemon] [text "Search"]
      ]
  else
    div [] []

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- HTTP

getPokemon : String -> Cmd Msg
getPokemon search =
  let
    url = "http://pokeapi.co/api/v2/pokemon/" ++ search ++ "/"
  in
    Task.perform FetchFail FetchSuccess (Http.get decodeResponse url)

decodeResponse : Decoder Pokemon
decodeResponse =
  object4 Pokemon
    (at ["name"] string)
    (at ["height"] int)
    (at ["weight"] int)
    (at ["sprites", "front_default"] string)
