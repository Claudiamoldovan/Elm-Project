module Model.PersonalDetails exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, id)

type alias DetailWithName =
    { name : String
    , detail : String
    }


type alias PersonalDetails =
    { name : String
    , contacts : List DetailWithName
    , intro : String
    , socials : List DetailWithName
    }
{-| Se introduc numele, intro, datele si linkurile
-}
view : PersonalDetails -> Html msg
view {name, intro, contacts, socials} = 
    let inputAttrs ty p v =[type_ty, placeholder p , value v]
    in
    div[]
    [h1[input (inputs id "name" name)][],
    em[input (inputs id "intro" intro)][],
    p[input ( inputs class "contact details" contacts)][],
    a[href class "social-link" socials] [] ]

statusView: PersonalDetails -> Html msg
statusView personalDetails =div[]
    [h1[][text"name", text PersonalDetails.name]
    ,em[][text"intro", text PersonalDetails.intro]
    ,p[][text "Contacts", class PersonalDetails.contacts]
    ,p[][text "Socials", class PersonalDetails.socials]
    ]

--contactsClass: Test
--contactsClass = \_ ->view 
  --  |> Q.fromHtml
    --|> Q.findAll [S.class "contact details"]
    --|> Q.count (Expect.atLeast 1)

--socialsClass: Test
--socialsClass = \_ ->view 
  --  |> Q.fromHtml
    --|> Q.findAll [S.class "social-link"]
   -- |> Q.count (Expect.atLeast 1)
