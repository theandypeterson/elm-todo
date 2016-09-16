import Html exposing(..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json

main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { list : List Todo
  , newItem : String
  , uid : Int
  }

type alias Todo =
  { id : Int
  , content : String
  , isChecked : Bool
  }


init : (Model, Cmd Msg)
init =
  (Model [] "" 1, Cmd.none)


type Msg
  = NoOp
  | AddToList
  | ChangeNewItem String
  | ClearList
  | CheckTodo Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

    AddToList ->
      if model.newItem /= "" then
        ({ model
         | list = (List.append model.list [(Todo model.uid model.newItem False)])
         , newItem = ""
         , uid = model.uid + 1
         }, Cmd.none)
      else
        (model, Cmd.none)

    ChangeNewItem item ->
      ({model | newItem = item}, Cmd.none)

    ClearList ->
      ({ model | list = [] }, Cmd.none)

    CheckTodo id ->
      let
        markTodo todo =
          if id == todo.id then
            { todo | isChecked = not todo.isChecked }
          else
            todo
       in
        ({ model | list = (List.map markTodo model.list)}, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


view : Model -> Html Msg
view model =
  div[]
    [ h1 [] [text "TODO LIST"]
    , div [] (List.map renderItem model.list)
    , fancyField model.newItem
    , removeAllButton
    ]

renderItem : Todo -> Html Msg
renderItem todo =
  div []
    [ input [type' "checkbox", checked todo.isChecked, onClick (CheckTodo todo.id)] []
    , if todo.isChecked then s [] [text todo.content] else text todo.content
    ]
fancyField : String -> Html Msg
fancyField content =
  div []
    [ input
        [ placeholder "Add something to the list"
        , onInput ChangeNewItem
        , onEnter AddToList
        , value content
        ] []
    ]

onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if code == 13 then msg else NoOp
  in
    on "keydown" (Json.map tagger keyCode)

removeAllButton : Html Msg
removeAllButton =
  button [ onClick ClearList ] [ text "Clear the list" ]

