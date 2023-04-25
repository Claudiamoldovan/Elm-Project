module Model.Date exposing (Date, Month(..), compare, compareMonth, full, month, monthToString, monthsBetween, monthsBetweenMonths, offsetMonths, onlyYear, view, year)

import Html exposing (Html, text)
import Model.Util exposing (chainCompare)


type Date
    = Date { year : Int, month : Maybe Month }


year : Date -> Int
year (Date d) =
    d.year


month : Date -> Maybe Month
month (Date d) =
    d.month


full : Int -> Month -> Date
full y m =
    Date { year = y, month = Just m }


onlyYear : Int -> Date
onlyYear y =
    Date { year = y, month = Nothing }

init : () -> ( Date, Cmd Msg )
init _ =
    ( {  year = "", month = ""}
    , Cmd.none
    )

{-| Am comparat prima data comezile daca sunt only sau full, dupa care 
daca este only doar scadem anii, iar daca este full se calculeaza in functie
de diferenta de ani cu formulele de mai jos sau in cazul in care este acelasi
an doar se scad lunile. Am folosit functia care transforma luna in int.
-}
monthsBetween : Date -> Date -> Maybe Int
monthsBetween dA dB =
    if onlyYear then 
    Just dB.year-dA.year
    else
    if full then 
    if dB.year ==  dA.year then 
    Just  dB.month-  dA.month
    else
    if dB.year>dA.year then
    if monthToInt dB.month > monthToInt dA.month then
    Just (dB.year-dA.year)*12-monthToInt dB.month +monthToInt dA.month
    else 
    Just (dB.year-dA.year)*12+monthToInt dA.month - monthToInt dB.month
    else
    Just monthToInt dB.month - monthToInt dA.month 
    else
    Nothing
{-| Am testat prima data daca anii sunt mai mari, deoarece clar daca anul A 
este mai mare decat anul B este mai mare si data, dupa care am testat si pentru 
luni in cazul in care anii sunt egali.
-}
compare : Date -> Date -> Order
compare (Date d1) (Date d2) =if onlyYear then 
    if d1.year > d2.year then 
    GT
    else
    LT
    else
    if d1.year> d2.year then 
    GT 
    else if d1.year < d2.year then 
    LT
    else
    EQ
    if monthToInt dB.month < monthToInt dA.month then
    GT
    else 
    if monthToInt dB.month > monthToInt dA.month then 
    LT
    else
    EQ
 
offsetMonths : Int -> Date -> Date
offsetMonths months (Date d) =
    let
        addMonths =
            modBy 12 months

        addedMonths =
            d.month
                |> Maybe.map monthToInt
                |> Maybe.map ((+) addMonths)

        newMonth =
            addedMonths
                |> Maybe.map (modBy 12)
                |> Maybe.andThen intToMonth

        addYears =
            months // 12

        extraYear =
            if Maybe.withDefault 0 addedMonths >= 12 then
                1

            else
                0
    in
    Date { year = d.year + addYears + extraYear, month = newMonth }

view1:Date->Html msg
view {Date}=
    let 
    inputAttrs ty p v =
    [type_ty, placeholder p , value v]
    in
    div[]
    [input (inputAttrs "text" "year" year)[]
    ,input(inputAttrs "text" "month" month)[]]

view : Date -> Html msg
view (Date d) =
    -- div [] 
    [view Date
    ]
	
    


-- MONTH


type Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec


intToMonth : Int -> Maybe Month
intToMonth idx =
    case idx of
        0 ->
            Just Jan

        1 ->
            Just Feb

        2 ->
            Just Mar

        3 ->
            Just Apr

        4 ->
            Just May

        5 ->
            Just Jun

        6 ->
            Just Jul

        7 ->
            Just Aug

        8 ->
            Just Sep

        9 ->
            Just Oct

        10 ->
            Just Nov

        11 ->
            Just Dec

        _ ->
            Nothing


monthToInt : Month -> Int
monthToInt m =
    case m of
        Jan ->
            0

        Feb ->
            1

        Mar ->
            2

        Apr ->
            3

        May ->
            4

        Jun ->
            5

        Jul ->
            6

        Aug ->
            7

        Sep ->
            8

        Oct ->
            9

        Nov ->
            10

        Dec ->
            11


monthToString : Month -> String
monthToString m =
    case m of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"

{-| nefiind vorrba si de ani, doar am comparat cele doua int uri rezultate 
din transformarea lunii in int 
-}
compareMonth : Month -> Month -> Order
compareMonth m1 m2 =
    a= MonthToInt m1
	b=MonthToInt m2
	if a > b then 
		GT
	else
		if a < b
		LT 
		else 
		EQ

{-| Diferenta dintre cele doua luni ofera rezultatul
-}
monthsBetweenMonths : Month -> Month -> Int
monthsBetweenMonths m1 m2 =
    a= MonthToInt m1
	b=MonthToInt m2
	b-a
