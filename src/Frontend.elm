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
    ( { key = key
      , message = "Welcome to Lamdera! You're looking at the auto-generated base implementation. Check out src/Frontend.elm to start coding!"
      , newItem = "Eric Voorhees"
      , rankingLists =
            Dict.fromList [ ( "default", [ "a", "b", "c" ] |> Array.fromList |> RankedList ) ]
                |> Debug.log "init"
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    let
        updateModel changeFn listName =
            Dict.get listName model.rankingLists
                |> Maybe.map changeFn
                |> Maybe.map (\newRankingList -> { model | rankingLists = Dict.insert listName newRankingList model.rankingLists })
                |> Maybe.withDefault model
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
            ( updateModel (increaseRanking index) listName
            , Cmd.none
            )

        DecreaseRanking listName index ->
            ( updateModel (decreaseRanking index) listName
            , Cmd.none
            )

        AddItem listName item ->
            ( updateModel (addItem item) listName
            , Cmd.none
            )

        RemoveItem listName index ->
            ( updateModel (removeIndex index) listName
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
        _ = Debug.log "" model
    in
    { title = ""
    , body =
        [ column
            []
            (drawAddItem model
                :: (model.rankingLists
                        |> Dict.keys
                        |> Debug.log "keys"
                        |> List.map (\i -> row [] [ Element.text i ])
                        
                   )
            )
            |> Element.layout []
        ]
    }


drawAddItem model =
    row []
        [ Element.Input.text []
            { placeholder = Element.text "Item name" |> Element.Input.placeholder [] |> Just
            , onChange = EditNewItem
            , text = model.newItem
            , label = labelHidden "Item name"
            }
        , Element.Input.button []
            { onPress = AddItem "default" model.newItem |> Just
            , label = Element.text "Add"
            }
        ]
