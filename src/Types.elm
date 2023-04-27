module Types exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import MoreDict exposing (..)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , message : String
    , newItem : String
    , rankingLists : Dict String RankedList
    }


type alias BackendModel =
    { message : String
    }


type RankedList
    = RankedList (Array String)


swapToIndexed : Int -> Int -> RankedList -> RankedList
swapToIndexed indexA indexB (RankedList rankedList) =
    let
        itemA =
            Array.get indexA rankedList

        itemB =
            Array.get indexB rankedList
    in
    case ( itemA, itemB ) of
        ( Nothing, _ ) ->
            RankedList rankedList

        ( _, Nothing ) ->
            RankedList rankedList

        ( Just itemA_, Just itemB_ ) ->
            rankedList
                |> Array.set indexA itemB_
                |> Array.set indexB itemA_
                |> RankedList


increaseRanking : Int -> RankedList -> RankedList
increaseRanking index ranking =
    if index == 0 then
        ranking

    else
        ranking
            |> swapToIndexed index (index - 1)


decreaseRanking : Int -> RankedList -> RankedList
decreaseRanking index (RankedList ranking) =
    if index == Array.length ranking - 1 then
        ranking
            |> RankedList

    else
        ranking
            |> RankedList
            |> swapToIndexed index (index + 1)


removeIndex : Int -> RankedList -> RankedList
removeIndex index (RankedList ranking) =
    let
        lengthOfLastBit =
            Array.length ranking - index - 1
    in
    ranking
        |> Array.slice 0 index
        |> Array.append (ranking |> Array.slice (index + 1) lengthOfLastBit)
        |> RankedList


addItem : String -> RankedList -> RankedList
addItem item (RankedList ranking) =
    ranking
        |> Array.append (Array.fromList [ item ])
        |> RankedList


toIndexMapping : RankedList -> Dict String Int
toIndexMapping (RankedList ranking) =
    ranking
        |> Array.indexedMap (\i v -> ( v, i ))
        |> Array.toList
        |> Dict.fromList

combineRanks : List RankedList -> Dict String Int
combineRanks rankedLists =
    let
        rankedListsIndexMapped = 
            List.map toIndexMapping rankedLists

        join left right =
            MoreDict.fullOuterJoin left right
                |> Dict.map
                    (\_ ( l, r ) ->
                        case ( l, r ) of
                            ( Just l_, Just r_ ) ->
                                l_ + r_

                            ( Just l_, Nothing ) ->
                                l_

                            ( Nothing, Just r_ ) ->
                                r_

                            ( Nothing, Nothing ) ->
                                -1
                    )

        joined = 
            List.foldl join Dict.empty rankedListsIndexMapped
    in
    joined 


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
      ----
    | IncreaseRanking String Int
    | DecreaseRanking String Int
    | AddItem String
    | RemoveItem Int
    | EditNewItem String


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
