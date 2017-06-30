module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode


main : Program Never Model Msg
main =
    Html.program
        { init = init ""
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
    , getWordFreqs text
    )



-- UPDATE


type Msg
    = MorePlease
    | NewGif (Result Http.Error String)
    | WordFreq (Result Http.Error WordMetadata)
    | Headline String


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

        WordFreq (Ok { word, freq }) ->
            ( { model
                | text =
                    if word == (Debug.log "Word: " word) then
                        model.text
                    else
                        model.text
                , keywords =
                    model.keywords
                        ++ (if (isFrequent (Debug.log "Freq: " freq)) then
                                [ Debug.log "Word: " word ]
                            else
                                []
                           )
              }
            , Cmd.none
            )

        WordFreq (Err _) ->
            ( model, Cmd.none )

        Headline text ->
            ( { model | text = text }, getWordFreqs text )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Headline" ]
        , input [ type_ "text", placeholder "News Headline", onInput Headline, style [ ( "width", "600px" ) ] ] []
        , h2 [] [ text "Keywords: " ]
        , p [] [ text (String.join " " model.keywords) ]
        , button [ onClick MorePlease ] [ text "Show Gifs!" ]
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


getWordFreqs : String -> Cmd Msg
getWordFreqs text =
    let
        words =
            String.split " " text
    in
        Cmd.batch (List.map getWordFreq words)


getWordFreq : String -> Cmd Msg
getWordFreq word =
    let
        url =
            "https://api.datamuse.com/words?md=f&max=1&sp=" ++ word
    in
        Http.send WordFreq (Http.get url decodeWordFreqUrl)


type alias WordMetadata =
    { word : String, freq : String }


decodeWordFreqUrl : Decode.Decoder WordMetadata
decodeWordFreqUrl =
    Decode.map2 WordMetadata
        (Decode.index 0 (Decode.field "word" Decode.string))
        (Decode.index 0 (Decode.field "tags" (Decode.index 0 Decode.string)))


isFrequent : String -> Bool
isFrequent freqStr =
    let
        threshold =
            150

        freq =
            Result.withDefault threshold (String.toFloat (String.dropLeft 2 freqStr))
    in
        (Debug.log "Parsed freq: " freq) < threshold
