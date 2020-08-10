port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Common exposing (mapIf)
import Html exposing (Html, a, button, div, footer, h1, header, input, label, li, section, span, text, ul)
import Html.Attributes exposing (autofocus, checked, class, classList, for, hidden, href, id, name, placeholder, type_, value)
import Html.Events exposing (onBlur, onCheck, onClick, onDoubleClick, onInput, onSubmit)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Task
import Todo exposing (Todo)


type alias Model =
    { newTodo : Todo
    , todos : List Todo
    , actualFilter : String
    }


initalModel : Model
initalModel =
    { newTodo = Todo.empty 0
    , todos = []
    , actualFilter = "All"
    }


type Msg
    = NoOp
    | Blur Int
    | ClearCompleted
    | ChangeFilter String
    | Check Int Bool
    | CheckAll Bool
    | Delete Int
    | Focus Int
    | Input (Maybe Int) String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Blur id ->
            let
                update_ =
                    mapIf (\t -> t.id == id) Todo.stopEditing
            in
            ( { model | todos = update_ model.todos }, Cmd.none )

        ClearCompleted ->
            let
                update_ =
                    List.filter (not << .completed)
            in
            ( { model | todos = update_ model.todos }, Cmd.none )

        ChangeFilter changeTo ->
            ( { model | actualFilter = changeTo }, Cmd.none )

        Check id checked ->
            let
                update_ =
                    mapIf (\t -> t.id == id) (Todo.check checked)
            in
            ( { model | todos = update_ model.todos }, Cmd.none )

        CheckAll checked ->
            let
                update_ =
                    List.map (Todo.check checked)
            in
            ( { model | todos = update_ model.todos }, Cmd.none )

        Delete id ->
            let
                update_ =
                    List.filter (\t -> t.id /= id)
            in
            ( { model | todos = update_ model.todos }, Cmd.none )

        Focus id ->
            let
                focusCmd =
                    Task.attempt (\_ -> NoOp) (Dom.focus ("todo-" ++ String.fromInt id))

                update_ =
                    mapIf (\t -> t.id == id) Todo.startEditing
            in
            ( { model | todos = update_ model.todos }, focusCmd )

        Input (Just id) name ->
            let
                update_ =
                    mapIf (\t -> t.id == id) (\t -> { t | name = name })
            in
            ( { model | todos = update_ model.todos }, Cmd.none )

        Input Nothing name ->
            let
                update_ t =
                    { t | name = name }
            in
            ( { model | newTodo = update_ model.newTodo }, Cmd.none )

        Submit ->
            ( { model
                | newTodo = Todo.empty (model.newTodo.id + 1)
                , todos = model.todos ++ [ model.newTodo ]
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "todomvc-wrapper" ]
        [ section [ class "todoapp" ]
            [ lazy viewHeader model.newTodo
            , lazy2 viewMain model.todos model.actualFilter
            , lazy2 viewFooter model.todos model.actualFilter
            ]
        ]


viewHeader : Todo -> Html Msg
viewHeader newTodo =
    header [ class "header" ] [ h1 [] [ text "todos" ], viewNewTodoInput newTodo ]


viewNewTodoInput : Todo -> Html Msg
viewNewTodoInput newTodo =
    Html.form [ onSubmit Submit ]
        [ input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , value newTodo.name
            , autofocus True
            , name "newTodo"
            , onInput (Input Nothing)
            ]
            []
        ]


viewMain : List Todo -> String -> Html Msg
viewMain todos actualFilter =
    let
        visibleTodos =
            case actualFilter of
                "Active" ->
                    todos |> List.filter (not << .completed)

                "Completed" ->
                    todos |> List.filter .completed

                _ ->
                    todos
    in
    section [ class "main", hidden (List.isEmpty todos) ]
        [ input
            [ id "toggle-all"
            , class "toggle-all"
            , type_ "checkbox"
            , name "toggle"
            , checked (List.all .completed todos)
            , onCheck CheckAll
            ]
            []
        , label [ for "toggle-all" ] [ text "Mark all as complete" ]
        , Keyed.ul [ class "todo-list" ] (visibleTodos |> List.map viewKeyedTodo)
        ]


viewKeyedTodo : Todo -> ( String, Html Msg )
viewKeyedTodo todo =
    ( String.fromInt todo.id, lazy viewTodo todo )


viewTodo : Todo -> Html Msg
viewTodo todo =
    li [ classList [ ( "completed", todo.completed ), ( "editing", todo.editing ) ] ]
        [ div [ class "view" ]
            [ input [ class "toggle", type_ "checkbox", checked todo.completed, onCheck (Check todo.id) ] []
            , label [ onDoubleClick (Focus todo.id) ] [ text todo.name ]
            , button [ class "destroy", onClick (Delete todo.id) ] []
            ]
        , Html.form [ onSubmit (Blur todo.id) ]
            [ input
                [ class "edit"
                , id ("todo-" ++ String.fromInt todo.id)
                , name "title"
                , value todo.name
                , onBlur (Blur todo.id)
                , onInput (Input (Just todo.id))
                ]
                []
            ]
        ]


viewFooter : List Todo -> String -> Html Msg
viewFooter todos actualFilter =
    footer [ class "footer", hidden (List.isEmpty todos) ]
        [ viewTodoCount todos
        , viewFilters actualFilter
        , viewCompletedCount todos
        ]


viewTodoCount : List Todo -> Html Msg
viewTodoCount todos =
    let
        activeCount =
            todos |> List.filter (\t -> not t.completed) |> List.length

        itemsLeftText =
            if activeCount == 1 then
                String.fromInt activeCount ++ " item left"

            else
                String.fromInt activeCount ++ " items left"
    in
    span [ class "todo-count" ] [ text itemsLeftText ]


viewFilters : String -> Html Msg
viewFilters actualFilter =
    ul
        [ class "filters" ]
        [ viewFilterLink actualFilter "All" "#/"
        , viewFilterLink actualFilter "Active" "#/active"
        , viewFilterLink actualFilter "Completed" "#/completed"
        ]


viewFilterLink : String -> String -> String -> Html Msg
viewFilterLink actual changeTo changeToRef =
    li
        [ onClick (ChangeFilter changeTo) ]
        [ a
            [ href changeToRef, classList [ ( "selected", actual == changeTo ) ] ]
            [ text changeTo ]
        ]


viewCompletedCount : List Todo -> Html Msg
viewCompletedCount todos =
    let
        completedCount =
            todos |> List.filter (\t -> t.completed) |> List.length |> String.fromInt
    in
    button [ class "clear-completed", onClick ClearCompleted ] [ text ("Clear completed (" ++ completedCount ++ ")") ]


main : Program (Maybe Model) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateModelAndStorage
        , subscriptions = \_ -> Sub.none
        }


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    ( Maybe.withDefault initalModel maybeModel, Cmd.none )


updateModelAndStorage : Msg -> Model -> ( Model, Cmd Msg )
updateModelAndStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel, Cmd.batch [ setStorage newModel, cmds ] )


port setStorage : Model -> Cmd msg

