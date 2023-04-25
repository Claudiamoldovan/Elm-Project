module Model.Repo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Json.Decode as De
import Json.Decode.Pipeline as JP

type alias Repo =
    { name : String
    , description : Maybe String
    , url : String
    , pushedAt : String
    , stars : Int
    }


init : () -> ( Repo, Cmd Msg )
init _ =
    ( { name="", description=Nothing, url="",pushedAt="", stars=""  }
    , Cmd.none
    )
{-|Se introduc detaliile
-}
view1:Repo->Html msg
view1 class Repo{name, description, url,pushedAt, stars}=
    let inputAttrs ty p v =[type_ty, placeholder p , value v]
    in
    div[]
    [input (inputAttrs class "repo-name" name )[]
    ,input(inputAttrs class  "repo-description" description)[]
    ,a[href class "repo-url" url ][]
    ,input(inputAttrs class "repo-stars" starts)[]
    ]
view : Repo -> Html msg
view repo =
    -- div [] 
    [ view1 Repo
    ]


sortByStars : List Repo -> List Repo
sortByStars repos =
    List.sortBy Repo.stars

{-| Am facut decodarea in doua varinate : prima cu pipe
line si alta normala
-}
decodeRepo : De.Decoder Repo
decodeRepo = JP.decode Repo
    |> JP.required "name" Decode.string
    |> JP.optional "description" (Decode.map Just Decode.string) Nothing
    |> JP.required "url" Decode.string
    |> JP.required "pushedAt" Decode.string
    |> JP.required "stars" Decode.int
decodeRepo2 : De.Decoder Repo
decodeRepo2 = Decode.map5 Repo
    (Decode.field "name" Decode.string)
    (Decode.maybe "description" Decode.string)
    (Decode.field "url" Decode.string)
    (Decode.field "pushedAt" Decode.string)
    (Decode.field "stars" Decode.int)