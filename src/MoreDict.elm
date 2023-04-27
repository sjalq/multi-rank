module MoreDict exposing (fullOuterJoin, leftOuterJoin, rightOuterJoin, innerJoin, nextId, insertNew, upsert)

import Dict exposing (Dict)

fullOuterJoin : Dict comparable l -> Dict comparable r -> Dict comparable (Maybe l, Maybe r)
fullOuterJoin left right =
    let
        leftKeys =
            Dict.keys left

        rightKeys =
            Dict.keys right

        leftNotInRight = 
            leftKeys |> List.filter (\k -> not <| Dict.member k right) 

        allKeys = 
            List.append leftNotInRight rightKeys

        joinedValues =
            List.map (\key -> ( key, ( Dict.get key left, Dict.get key right ) )) allKeys
    in 
    Dict.fromList joinedValues


getLeftOuter : Dict comparable (l, Maybe r) -> Dict comparable (l, r)
getLeftOuter joinedDict =
    joinedDict
        |> Dict.foldl 
            (\key (l, mayber) acc ->
                case mayber of
                    Just value ->
                        Dict.insert key (l, value) acc

                    Nothing ->
                        acc
            )
            Dict.empty
            

getRightOuter : Dict comparable (Maybe l, r) -> Dict comparable (l, r)
getRightOuter joinedDict =
    joinedDict
        |> Dict.foldl 
            (\key (maybel, r) acc ->
                case maybel of
                    Just value ->
                        Dict.insert key (value, r) acc

                    Nothing ->
                        acc
            )
            Dict.empty
        

getInner : Dict comparable (Maybe a, Maybe b) -> Dict comparable (a, b)
getInner joinedDict =
    joinedDict |> 
        (getLeftOuter >> getRightOuter)


leftOuterJoin : Dict comparable l -> Dict comparable r -> Dict comparable (Maybe l, r)
leftOuterJoin left right =
    fullOuterJoin left right
        |> getLeftOuter


rightOuterJoin : Dict comparable l -> Dict comparable r -> Dict comparable (l, Maybe r)
rightOuterJoin left right =
    fullOuterJoin left right
        |> getRightOuter


innerJoin : Dict comparable a -> Dict comparable b -> Dict comparable (a, b)
innerJoin left right =
    fullOuterJoin left right
        |> getInner


nextId : Dict.Dict number v -> number
nextId dict =
    Dict.keys dict
        |> List.maximum
        |> Maybe.withDefault -1
        |> (+) 1


insertNew : { a | id : number } -> Dict.Dict number { a | id : number } -> (Dict.Dict number { a | id : number }, { a | id : number })
insertNew item dict =
    let
        id =
            nextId dict 

        itemWithNewId =
            { item | id = id }
    in
    (dict |> Dict.insert id itemWithNewId, itemWithNewId)


upsert : { a | id : number } -> Dict.Dict number { a | id : number } -> (Dict.Dict number { a | id : number }, { a | id : number })
upsert item dict =
    if item.id <= -1 then
        dict |> insertNew item

    else
        (dict |> Dict.insert item.id item, item)
