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
  , title : String
  , isEditingTitle : Bool
  , newTitle : String
  }


type alias Todo =
  { id : Int
  , content : String
  , isChecked : Bool
  }


init : (Model, Cmd Msg)
init =
  (Model [] "" 1 "TODO LIST" False "", Cmd.none)


type Msg
  = NoOp
  | AddToList
  | ChangeNewItem String
  | ClearList
  | CheckTodo Int
  | ToggleTitleEdit
  | UpdateTitle
  | ChangeNewTitle String
  | ClearCompleted


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

    ToggleTitleEdit ->
      ({ model | isEditingTitle = (not model.isEditingTitle), newTitle = model.title }, Cmd.none)

    UpdateTitle ->
      ({ model | title = model.newTitle, isEditingTitle = False}, Cmd.none)

    ChangeNewTitle newTitle ->
      ({ model | newTitle = newTitle }, Cmd.none)

    ClearCompleted ->
      let
        notComplete todo =
          not todo.isChecked
      in
        ({ model | list = (List.filter notComplete model.list) }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


view : Model -> Html Msg
view model =
  div[]
    [ renderTitle model
    , div [] (List.map renderItem model.list)
    , fancyField model.newItem
    , div []
      [ removeAllButton
      , removeCompletedButton
      ]
    ]

renderTitle : Model -> Html Msg
renderTitle model =
  if model.isEditingTitle then
    input
      [ value model.newTitle
      , autofocus True
      , onEnter UpdateTitle
      , onInput ChangeNewTitle
      ] []
  else
    h1 [onClick ToggleTitleEdit] [text model.title]


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


removeCompletedButton : Html Msg
removeCompletedButton =
  button [ onClick ClearCompleted ] [ text "Clear completed" ]
