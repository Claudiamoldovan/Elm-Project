module VerifyExamples.Model.Event.Category.IsEventCategorySelected0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Model.Event.Category exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#isEventCategorySelected: \n\n    isEventCategorySelected Academic allSelected\n    --> True" <|
        \() ->
            Expect.equal
                (
                isEventCategorySelected Academic allSelected
                )
                (
                True
                )