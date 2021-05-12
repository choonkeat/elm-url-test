module Main exposing (main)

import Browser
import Browser.Navigation
import Html exposing (Html, datalist, div, input, label, option, p, text)
import Html.Attributes exposing (checked, href, id, list, name, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Url
import Url.Builder
import Url.Parser exposing ((</>), (<?>), int, map, s, string)
import Url.Parser.Query



--


type DecodeMode
    = NoPercentEncodeDecode
    | PercentEncodeDecodePath


buildAbsolute : DecodeMode -> List String -> List Url.Builder.QueryParameter -> String
buildAbsolute mode paths params =
    case mode of
        NoPercentEncodeDecode ->
            Url.Builder.absolute
                paths
                params

        PercentEncodeDecodePath ->
            -- https://github.com/elm/url/issues/25
            Url.Builder.absolute
                (List.map Url.percentEncode paths)
                params


parserString : DecodeMode -> Url.Parser.Parser (String -> a) a
parserString mode =
    case mode of
        NoPercentEncodeDecode ->
            Url.Parser.string

        PercentEncodeDecodePath ->
            -- https://github.com/elm/url/issues/16
            Url.Parser.custom "STRING" Url.percentDecode



--


type Page
    = Page String String


router : DecodeMode -> Url.Parser.Parser (Page -> b) b
router mode =
    Url.Parser.oneOf
        [ Url.Parser.map Page
            (s "path"
                </> parserString mode
                <?> (Url.Parser.Query.string "query" |> Url.Parser.Query.map (Maybe.withDefault ""))
            )
        ]


linkTo : DecodeMode -> String -> String -> Html msg
linkTo mode path query =
    let
        params =
            if query == "" then
                []

            else
                [ Url.Builder.string "query" query ]

        url =
            buildAbsolute mode [ "path", path ] params
    in
    Html.a [ href url ]
        [ text
            ("absolute "
                ++ Debug.toString [ "path", path ]
                ++ (" [ string \"query\" "
                        ++ Debug.toString query
                        ++ "]"
                   )
                ++ " = "
                ++ Debug.toString url
            )
        ]



--


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


type alias Model =
    { navKey : Browser.Navigation.Key
    , url : Url.Url
    , mode : DecodeMode
    , page : Maybe Page
    , path : String
    , query : String
    }


updatePage : Url.Url -> Model -> Model
updatePage url model =
    { model | url = url, page = Url.Parser.parse (router model.mode) url }


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url.Url
    | OnDecodeMode DecodeMode
    | OnPathInput String
    | OnQueryInput String


init : {} -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    ( updatePage url
        { navKey = navKey
        , url = url
        , mode = NoPercentEncodeDecode
        , page = Nothing
        , path = ""
        , query = ""
        }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    Browser.Document "App"
        [ Html.div [ style "margin" "10em", style "font-family" "monospace" ]
            [ Html.p []
                [ p []
                    [ text "Step 1. Set Path "
                    , input [ onInput OnPathInput, value model.path, placeholder "path", list "path-input" ] []
                    , autocompleteFor "path-input"
                    ]
                , p []
                    [ text "Step 2. Set Query"
                    , input [ onInput OnQueryInput, value model.query, placeholder "query", list "query-input" ] []
                    , autocompleteFor "query-input"
                    ]
                , p []
                    [ text "Step 3. Set Mode "
                    , modeRadioButton NoPercentEncodeDecode model
                    , modeRadioButton PercentEncodeDecodePath model
                    ]
                , p [] [ text "Step 4. Click ", linkTo model.mode model.path model.query ]
                ]
            , if model.page == Nothing then
                text ""

              else
                div []
                    [ div [] [ text ("Expect : " ++ Debug.toString (Just (Page model.path model.query))) ]
                    , div [] [ text ("Actual : " ++ Debug.toString model.page) ]
                    ]
            ]
        ]


modeRadioButton : DecodeMode -> Model -> Html Msg
modeRadioButton decodeMode model =
    label [] [ input [ onClick (OnDecodeMode decodeMode), type_ "radio", checked (model.mode == decodeMode) ] [], text (decodeModeString decodeMode) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlRequest (Browser.Internal url) ->
            ( model, Browser.Navigation.pushUrl model.navKey (Url.toString url) )

        OnUrlRequest (Browser.External urlString) ->
            ( model, Browser.Navigation.load urlString )

        OnUrlChange url ->
            ( updatePage url model, Cmd.none )

        OnDecodeMode mode ->
            ( { model | mode = mode, page = Nothing }, Cmd.none )

        OnPathInput string ->
            ( { model | path = string, page = Nothing }, Cmd.none )

        OnQueryInput string ->
            ( { model | query = string, page = Nothing }, Cmd.none )



--


decodeModeString : DecodeMode -> String
decodeModeString mode =
    case mode of
        NoPercentEncodeDecode ->
            "NoPercentEncodeDecode"

        PercentEncodeDecodePath ->
            "PercentEncodeDecodePath"


autocompleteFor : String -> Html msg
autocompleteFor string =
    datalist [ id string ]
        [ option [ value "hello" ] []
        , option [ value "👍" ] []
        , option [ value "!@#$%^&*()" ] []
        , option [ value "{}[]<>;/" ] []
        , option [ value "日本" ] []
        , option [ value "خحجث" ] []
        ]
