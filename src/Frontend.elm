module Frontend exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input exposing (..)
import Lamdera
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    let
        _ =
            Debug.log "app" "app"
    in
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        _ =
            Debug.log "init" url
    in
    ( { key = key
      , message = "Welcome to Lamdera! You're looking at the auto-generated base implementation. Check out src/Frontend.elm to start coding!"
      , newItem = ""
      , rankingLists =
            Dict.fromList
                [ ( "Difficulty", [] |> Array.fromList |> RankedList )
                , ( "Cost", [] |> Array.fromList |> RankedList )
                , ( "Impact", [] |> Array.fromList |> RankedList )
                ]
                |> Debug.log "init"
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    let
        updateModel changeFn listName model_ =
            Dict.get listName model_.rankingLists
                |> Maybe.map changeFn
                |> Maybe.map (\newRankingList -> { model_ | rankingLists = Dict.insert listName newRankingList model_.rankingLists })
                |> Maybe.withDefault model_
    in
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        IncreaseRanking listName index ->
            ( updateModel (increaseRanking index) listName model
            , Cmd.none
            )

        DecreaseRanking listName index ->
            ( updateModel (decreaseRanking index) listName model
            , Cmd.none
            )

        AddItem item ->
            let
                hasItem =
                    model.rankingLists
                        |> Dict.values
                        |> List.map (\(RankedList items) -> items |> Array.toList)
                        |> List.concat
                        |> List.member item

                newModel =
                    if hasItem then
                        model

                    else
                        model
                            |> updateModel (addItem item) "Impact"
                            |> updateModel (addItem item) "Difficulty"
                            |> updateModel (addItem item) "Cost"
            in
            ( newModel
            , Cmd.none
            )

        RemoveItem index ->
            ( model
                |> updateModel (removeIndex index) "Impact"
                |> updateModel (removeIndex index) "Difficulty"
                |> updateModel (removeIndex index) "Cost"
            , Cmd.none
            )

        EditNewItem item ->
            ( { model | newItem = item }
            , Cmd.none
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    let
        _ =
            Debug.log "" model
    in
    { title = ""
    , body =
        [ column
            []
            (drawAddItem model
                :: (model.rankingLists
                        |> Dict.map drawRankedList
                        |> Dict.values
                   )
                ++ [ drawCombinedRank model ]
            )
            |> Element.layout []
        ]
    }


tableSectionStyle =
    [ paddingEach { top = 20, bottom = 10, left = 0, right = 0 }
    , fill |> minimum 1000 |> width
    ]


headerAttrs =
    [ Font.bold
    , Font.color color.blue
    , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
    , Border.color color.lightGrey
    ]


drawRankedList : String -> RankedList -> Element FrontendMsg
drawRankedList listName (RankedList items) =
    column tableSectionStyle
        [ row []
            [ Element.text listName ]
        , table
            [ width fill, spacing 10 ]
            { data =
                items
                    |> Array.toList
                    |> List.indexedMap (\index item -> { index = index, item = item })
            , columns =
                [ { header = el headerAttrs <| Element.text "Item"
                  , width = fillPortion 2
                  , view =
                        \rec ->
                            row []
                                [ Element.Input.button buttonStyle
                                    { onPress = IncreaseRanking listName rec.index |> Just
                                    , label = Element.text "⬆️"
                                    }
                                , Element.Input.button buttonStyle
                                    { onPress = DecreaseRanking listName rec.index |> Just
                                    , label = Element.text "⬇️"
                                    }
                                , Element.text rec.item |> el [ centerX ]
                                , Element.Input.button (buttonStyle ++ [ alignRight ])
                                    { onPress = RemoveItem rec.index |> Just
                                    , label = Element.text "🗑️"
                                    }
                                ]

                  -- .item >> Element.text >> el [ centerY ]
                  }
                , { header = el headerAttrs <| Element.text "Rank"
                  , width = fillPortion 1
                  , view = .index >> (String.fromInt >> Element.text) >> el [ centerY ]
                  }
                ]
            }
        ]


drawCombinedRank model =
    let
        combinedRanks =
            model.rankingLists
                |> Dict.values
                |> combineRanks
    in
    column tableSectionStyle
        [ row []
            [ Element.text "Combined Rank" ]
        , table
            [ width fill, spacing 10 ]
            { data =
                combinedRanks
                    |> Dict.toList
                    |> List.sortBy Tuple.second
                    |> List.map
                        (\( item, combinedRank ) ->
                            { index = combinedRank, item = item }
                        )
            , columns =
                [ { header = el headerAttrs <| Element.text "Item"
                  , width = fillPortion 2
                  , view = .item >> Element.text >> el [ centerY ]
                  }
                , { header = el headerAttrs <| Element.text "Rank"
                  , width = fillPortion 1
                  , view = .index >> (String.fromInt >> Element.text) >> el [ centerY ]
                  }
                ]
            }
        ]


drawAddItem model =
    row []
        [ Element.Input.text []
            { placeholder = Element.text "Item name" |> Element.Input.placeholder [] |> Just
            , onChange = EditNewItem
            , text = model.newItem
            , label = labelHidden "Item name"
            }
        , Element.Input.button buttonStyle
            { onPress = AddItem model.newItem |> Just
            , label = Element.text "Add"
            }
        ]


buttonStyle =
    [ padding 5
    , Background.color color.lightBlue
    , Border.width 2
    , Border.rounded 5
    , Border.color color.blue
    , Border.shadow
        { offset = ( 4, 4 ), size = 3, blur = 10, color = color.lightGrey }
    , Font.color color.white
    , mouseDown
        [ Background.color color.white, Font.color color.darkCharcoal ]
    , focused
        [ Border.shadow
            { offset = ( 4, 4 ), size = 3, blur = 10, color = color.blue }
        ]
    ]


color =
    { blue = rgb255 0x72 0x9F 0xCF
    , darkCharcoal = rgb255 0x2E 0x34 0x36
    , green = rgb255 0x20 0xBF 0x55
    , lightBlue = rgb255 0xC5 0xE8 0xF7
    , lightGrey = rgb255 0xE0 0xE0 0xE0
    , orange = rgb255 0xF2 0x64 0x19
    , red = rgb255 0xAA 0x00 0x00
    , white = rgb255 0xFF 0xFF 0xFF
    }
