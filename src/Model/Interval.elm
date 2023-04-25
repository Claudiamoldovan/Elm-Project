module Model.Interval exposing (Interval, compare, full, length, oneYear, open, view, withDurationMonths, withDurationYears)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, style)
import Model.Date as Date exposing (Date, Month)
import Model.Util exposing (chainCompare)


type Interval
    = Interval { start : Date, end : Maybe Date }

full : Date -> Date -> Maybe Interval
full start end =
    if Date.compare start end == GT then
        Nothing

    else
        Just <| Interval { start = start, end = Just end }


withDurationMonths : Int -> Month -> Int -> Interval
withDurationMonths startYear startMonth duration =
    let
        start =
            Date.full startYear startMonth

        end =
            Date.offsetMonths duration start
    in
    Interval { start = start, end = Just end }


withDurationYears : Date -> Int -> Interval
withDurationYears start duration =
    let
        end =
            Date.offsetMonths (duration * 12) start
    in
    Interval { start = start, end = Just end }
open:Date -> Interval
open start =
    Interval { start = start, end = Nothing }


oneYear : Int -> Interval
oneYear year =
    withDurationYears (Date.onlyYear year) 1


length : Interval -> Maybe ( Int, Int )
length (Interval interval) =
    interval.end
        |> Maybe.andThen (Date.monthsBetween interval.start)
        |> Maybe.map (\totalMonths -> ( totalMonths // 12, modBy 12 totalMonths ))

{-|Am implementat aceasta functie pe considerentu ca intervalul este 
introdus sub forma unei tuple si pentru a se putea extrage capetele pentru 
implementarea functiei compare. In functia compare am comparat daca inceputul 
este la fel, atunci totul depinde de sfarsit, iar daca nu am luat si am 
calculat lungimea fiecarui interval 
-}
compare : Interval -> Interval -> Order
compare (Interval intA) (Interval intB) =if intA.start == intB.start then
    if intB.end> intA.end then 
    LT
    else
    GT
    else
    let lambdaFirst = \(x, y) -> x
    in 
    if lambdaFirst length intA> lambdaFirst length intB then 
    GT
    else 
    let lambdaSecond = \(x, y) -> y
    in 
    if lambdaFirst length intA< lambdaFirst length intB then 
    LT
    else
    if lambdaSecond length intA > lambdaSecond lenght intB then 
    GT
    else
    if lambdaSecond length intA < lambdaSecond lenght intB then 
    LT
    else EQ

init : () -> ( Interval, Cmd Msg )
init _ =
    ( {  start = "", end = "" }
    , Cmd.none
    )
{-| Pentru a se introduce starul si endul, iar daca endul nu este atunci 
se trece Present, iar daca este se face lungimea
-}
view1 : Interval -> Html msg
view1 interval class{interval} =
    let inputAttrs ty p v =[type_ty, placeholder p , value v]
    in
    div []
    [input (inputAttrs class "interval-start" start)[]
    , input (inputAttrs class "interval-end" end )[]
    ,if end == "" then 
    text "Present"
    else
    output (inputAttrs class "interval-lenght year"  length Interval stard end)[]
    ]
view :Interval ->Html Msg
view Interval =div[]
    [view1 Inteval
    ]

