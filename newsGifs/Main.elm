module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode


main : Program Never Model Msg
main =
    Html.program
        { init = init "Sinkhole swallows car in St Louis"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { text : String
    , keywords : List String
    , gifUrls : List String
    }


init : String -> ( Model, Cmd Msg )
init text =
    ( Model text [] []
    , getRandomGifs keywords
    )



-- UPDATE


type Msg
    = MorePlease
    | NewGif (Result Http.Error String)
    | WordFreq (Result Http.Error ( String, Float ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( { model | gifUrls = [] }
            , getRandomGifs model.keywords
            )

        NewGif (Ok newUrl) ->
            ( { model | gifUrls = model.gifUrls ++ [ newUrl ] }
            , Cmd.none
            )

        NewGif (Err _) ->
            ( model, Cmd.none )

        WordFreq (Ok ( word, freq )) ->
            ( model, Cmd.none )

        WordFreq (Err _) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text (String.join " " model.keywords) ]
        , button [ onClick MorePlease ] [ text "More Please!" ]
        , renderGifs model.gifUrls
        ]


renderGif : String -> Html Msg
renderGif gifUrl =
    span []
        [ img [ src gifUrl ] []
        ]


renderGifs : List String -> Html Msg
renderGifs gifUrls =
    let
        gifs =
            List.map renderGif gifUrls
    in
        div [] gifs



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getRandomGif : String -> Cmd Msg
getRandomGif keyword =
    let
        apiKey =
            "ab6500c0e00b495bad51326f11db0074"

        url =
            "https://api.giphy.com/v1/gifs/random?api_key=" ++ apiKey ++ "&rating=G&tag=" ++ keyword
    in
        Http.send NewGif (Http.get url decodeGifUrl)


getRandomGifs : List String -> Cmd Msg
getRandomGifs keywords =
    Cmd.batch (List.map getRandomGif keywords)


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
    Decode.at [ "data", "fixed_height_downsampled_url" ] Decode.string


getWordFreq : String -> Cmd Msg
getWordFreq word =
    let
        url =
            "https://api.datamuse.com/words?md=f&max=1&sp=" ++ word
    in
        Http.send WordFreq (Http.get url decodeWordFreqUrl)


decodeWordFreqUrl : Decode.Decoder String
decodeWordFreqUrl =
    ( Decode.index 0 (Decode.field "word" Decode.string)
    , Decode.index 0 (Decode.field "tags" (Decode.index 0 (Decode.field Decode.string)))
    )


isFrequent : String -> Bool
isFrequent word =
    True
