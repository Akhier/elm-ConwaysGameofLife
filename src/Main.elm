module Main exposing (main)

import Array exposing (Array)
import Browser exposing (element)
import Html exposing (..)
import Html.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Time exposing (every)


-- MODEL

type Cell
  = Dead
  | Alive

type Msg
  = Reset
  | ToggleRunning
  | NextGeneration
  | ToggleAt Int Int

type alias Grid =
  { cells : Array Cell, width : Int, height : Int }

type alias Model =
  { grid : Grid,
    running : Bool,
    generations : Int }

makeGrid : Int -> Int -> Grid
makeGrid width height =
  { cells = Array.repeat (width * height) Dead,
    width = width,
    height = height }

toggleCell : Int -> Int -> Grid -> Grid
toggleCell x y grid =
  let i = x + y * grid.width
  in
    case Array.get i grid.cells of
      Just cell ->
        case cell of
          Alive -> { grid | cells = Array.set i Dead grid.cells }
          Dead -> { grid | cells = Array.set i Alive grid.cells }
      Nothing -> grid

countLiveNeighbors : Int -> Grid -> Int
countLiveNeighbors cellIndex grid =
  let up = cellIndex - grid.width

      down = cellIndex + grid.width

      neighborCoords =
        [ up - 1, up, up + 1,
          cellIndex - 1, cellIndex + 1,
          down - 1, down, down + 1 ]
            |> List.filter (\n -> abs (modBy grid.width n - modBy grid.width cellIndex) <= 1)

      neighbors = List.map (\pos -> Array.get pos grid.cells) neighborCoords
  in List.length (List.filter (\n -> n == Just Alive) neighbors)

nextGenerationAt : Grid -> Int -> Cell -> Cell
nextGenerationAt grid i cell =
  let liveNeighbors = countLiveNeighbors i grid
  in
    case cell of
      Alive ->
        if liveNeighbors < 2 || liveNeighbors > 3 then
          Dead
        else Alive
      Dead ->
        if liveNeighbors == 3 then
          Alive
        else Dead

nextGeneration : Grid -> Grid
nextGeneration grid =
  { grid | cells = Array.indexedMap (nextGenerationAt grid) grid.cells }

init : ( Model, Cmd Msg )
init = ( { grid = makeGrid 50 50, running = False, generations = 0 }, Cmd.none )


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Reset ->
      init
    ToggleRunning ->
      ( { model | running = not model.running }, Cmd.none )
    NextGeneration ->
      let
        nextGrid = nextGeneration model.grid
        changed = nextGrid /= model.grid
      in
        ( { model |
            grid = nextGrid,
            running = model.running && changed,
            generations = model.generations + 1 },
          Cmd.none )
    ToggleAt x y ->
      ( { model | grid = toggleCell x y model.grid }, Cmd.none )


-- VIEW

resetLink : Html Msg
resetLink = Html.button [ Html.Events.onClick Reset ] [ Html.text "Reset"]

toggleRunningLink : Model -> Html Msg
toggleRunningLink model =
  Html.button [ Html.Events.onClick ToggleRunning ]
    [ Html.text
      ( if model.running then
          "Stop"
        else "Start" ) ]

nextGenerationLink : Model -> Html Msg
nextGenerationLink model =
  Html.button [ Html.Events.onClick NextGeneration ]
    [ Html.text "Next Generation" ]

cellSize : Int
cellSize = 10

flattenGrid : (Int -> Int -> Cell -> a) -> Grid -> List a
flattenGrid f g =
    List.map (\( i, c ) -> f (modBy g.width i) (i // g.width) c) (Array.toIndexedList g.cells)

cellToSvg : Int -> Int -> Cell -> Svg Msg
cellToSvg x_ y_ cell =
  let
    color =
      case cell of
        Alive -> "black"
        Dead -> "white"
  in
    rect
      [ x ( String.fromInt ( cellSize * x_ )),
        y ( String.fromInt ( cellSize * y_ )),
        width ( String.fromInt cellSize ),
        height ( String.fromInt cellSize ),
        fill color,
        stroke "#ddd",
        onClick ( ToggleAt x_ y_ ) ]
      []

view : Model -> Html Msg
view model =
  div []
    [ header []
      [ resetLink,
        toggleRunningLink model,
        nextGenerationLink model,
        Html.text ("Generations: " ++ String.fromInt model.generations ) ],
      svg
        [ width "50%",
          height "50%",
          viewBox
            ( "0 0 " ++
              String.fromInt (cellSize * model.grid.width) ++
              " " ++
              String.fromInt (cellSize * model.grid.height) ) ]
        ( flattenGrid cellToSvg model.grid ) ]

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
      every 100 (always NextGeneration)
    else
      Sub.none

main : Program () Model Msg
main =
  Browser.element
    { init = always init,
      view = view,
      update = update,
      subscriptions = subscriptions }