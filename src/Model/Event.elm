module Model.Event exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Model.Event.Category exposing (EventCategory(..))
import Model.Interval as Interval exposing (Interval)


type alias Event =
    { title : String
    , interval : Interval
    , description : Html Never
    , category : EventCategory
    , url : Maybe String
    , tags : List String
    , important : Bool
    }


categoryView : EventCategory -> Html Never
categoryView category =
    case category of
        Academic ->
            text "Academic"

        Work ->
            text "Work"

        Project ->
            text "Project"

        Award ->
            text "Award"

{-| Se sorteaza dupa lungimea intervalului
-} 
sortByInterval : List Event -> List Event
sortByInterval events =
    List.sortBy List.length Event.Interval
{-|Am implementat inputurile pentru a trece titlul, categoria, descrierea
si url 
-}
detailsView: Model -> Html Msg
detailsView class Event {title, interval, description,category, url,tags, important}=
    let inputAttrs ty p v =[type_ty, placeholder p , value v]
    in
    div[]
    [input (inputAttrs class "eventTitle" title )[]
    ,input (inputAttrs class "event-description" description)[]
    ,input (inputAttrs class "event-category" category)[]
    ,input (inputAttrs class "event-url" url)[]
    ]
{-| Am folosit un check box in care sa se selecteze daca este important, 
iar daca este atunci sa se mai faca un input 
-}
activeImportant : Model -> Html Msg
activeImportant yes =
    div[]
    [input [ type_ "checkbox", onCheck True , check yes][]
    ,text "It's important?"
    ,if onCheck == TRUE then 
    let inputAttrs ty p v =[type_ty, placeholder p , value v]
    in
     input (inputAttrs class "event-important" important)[]
     else
     Norhing]

{-| Aici se vor afisa detaliile luate din model
-}
statusView : Model -> Html Msg
statusView model =
    div []
    [ p [] [ text "Titlu: ", text model.title ]
    , p [] [ text "Description: ", text model.description]
    , p [] [ text "Category: ", text model.category ]
    , p [] [ text "Url: ", text model.url ]
    ,if model.activeImportant then
    let inputAttrs ty p v =[type_ty, placeholder p , value v]
    in
    p[input (inputAttrs class "event-importnat" important)][]
    else
    p[][text "ok"]]
       

view : Event -> Html Never
view event =
    -- div [] 
     [detailsView model
    activeImportant model.important
     ]
   
