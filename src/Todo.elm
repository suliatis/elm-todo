module Todo exposing (Todo, check, empty, startEditing, stopEditing)


type alias Todo =
    { id : Int
    , name : String
    , completed : Bool
    , editing : Bool
    }


empty : Int -> Todo
empty id =
    { id = id
    , name = ""
    , completed = False
    , editing = False
    }


edit : Bool -> Todo -> Todo
edit editing todo =
    { todo | editing = editing }


startEditing : Todo -> Todo
startEditing =
    edit True


stopEditing : Todo -> Todo
stopEditing =
    edit False


check : Bool -> Todo -> Todo
check checked todo =
    { todo | completed = checked }
