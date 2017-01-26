module Xml.Decode exposing (toJson)


{-| The Decoder converts an XML AST to an Json value

@docs toJson
-}

import Tuple exposing (first, second)
import Json.Encode exposing (string, object, list, array)
import Xml.Parser exposing (XmlAst(..))
--import List.Extra as List
import Array


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
  case list of
    []      -> []
    x::xs   -> if (predicate x) then x :: takeWhile predicate xs
               else []


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
  case list of
    []      -> []
    x::xs   -> if (predicate x) then dropWhile predicate xs
               else list


span : (a -> Bool) -> List a -> (List a, List a)
span p xs = (takeWhile p xs, dropWhile p xs)


groupBy : (a -> a -> Bool) -> List a -> List (List a)
groupBy eq xs1 =
  case xs1 of
    [] -> []
    (x::xs) -> let (ys,zs) = span (eq x) xs
               in (x::ys)::groupBy eq zs


getElementsOrArray : List XmlAst -> List ( String, Json.Encode.Value )
getElementsOrArray elems =
  let
    -- partition the list by determining if a given key exists more than one time -> array
    ( array1, elems1 ) =
      elems
        |> List.map toJson1
        |> groupBy (\a b -> first a == first b)
        |> List.partition (\ls -> List.length ls > 1)

    -- flatten the "normal" elements
    elems2 =
      elems1
        |> List.concatMap identity

    -- create the array elements
    array2 =
      array1
        |> List.map
            (\ls ->
              let
                name =
                  ls
                    |>
                      List.head
                    |>
                      Maybe.map first
                    -- cannot happen, there is always at least one element
                    -- in the list by the nature of how groupBy works
                    |>
                      Maybe.withDefault ""
              in
                ( name
                , ls
                    |> List.map second
                    |> Array.fromList
                    |> array
                )
            )
  in
    elems2 ++ array2


getAttributes : List ( String, String ) -> List ( String, Json.Encode.Value )
getAttributes attrs =
  attrs
    |> List.map (\( n, v ) -> ( "__" ++ n, string v ))


toJson1 : XmlAst -> ( String, Json.Encode.Value )
toJson1 xmlAst =
  case xmlAst of
    Body txt ->
      ( "__text", string txt )

    Comment comment ->
      ( "__comment", string comment )

    CDATA cdata ->
      ( "__cdata", string cdata )

    Element name [] [ Body txt ] ->
      ( name, string txt )

    Element name attrs elems ->
      ( name, object (getAttributes attrs ++ getElementsOrArray elems) )


{-| Converts an XML AST to a Json value -}
toJson : List XmlAst -> Json.Encode.Value
toJson xmlAst =
  xmlAst
    |> List.map toJson1
    |> object
