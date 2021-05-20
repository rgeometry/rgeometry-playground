module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Fps
import Html exposing (Html)
import Html.Attributes as Attr exposing (class, disabled, href, id, rel, src, style, title, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Keyboard exposing (RawKey)
import List
import Platform.Sub
import Ports
import Task
import Time
import WebSocket


type Backend
    = Production
    | Local


backend : Backend
backend =
    Production


wsBackend : String
wsBackend =
    case backend of
        Production ->
            "wss://reanimate.clozecards.com:20443/ws/"

        Local ->
            "ws://localhost:20162/"


webBackend : String
webBackend =
    case backend of
        Production ->
            "https://reanimate.clozecards.com:20443/"

        Local ->
            "http://localhost:10162/"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Platform.Sub.batch
        [ Ports.receiveSocketMsg (WebSocket.receive MessageReceived)
        , Ports.receiveEditorMsg Change
        , Ports.receiveControlMsg parseControlMsg
        , case model of
            Problem ConnectionFailed ->
                Time.every 2000 (always AttemptReconnect)

            _ ->
                Sub.none
        ]


parseControlMsg : String -> Msg
parseControlMsg msg =
    case msg of
        _ ->
            NoOp


type Msg
    = MessageReceived (Result Json.Decode.Error WebSocket.WebSocketMsg)
    | AttemptReconnect
    | NoOp
    | Change String


type Model
    = Disconnected
    | Connected
    | Compiling
    | Running String
    | Problem Problem


type Problem
    = CompilationError String
    | ConnectionFailed
    | PortMessageDecodeFailure Json.Decode.Error
    | UnexpectedMessage String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Disconnected, connectCommand )


connectCommand : Cmd msg
connectCommand =
    WebSocket.send Ports.sendSocketCommand <|
        WebSocket.Connect
            { name = "TheSocket"
            , address = wsBackend
            , protocol = ""
            }


sendSource : String -> Cmd msg
sendSource txt =
    WebSocket.send Ports.sendSocketCommand <|
        WebSocket.Send
            { name = "TheSocket"
            , content = txt
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MessageReceived result ->
            ( processResult result model, Cmd.none )

        AttemptReconnect ->
            ( model, connectCommand )

        NoOp ->
            ( model, Cmd.none )

        Change txt ->
            ( Compiling, sendSource txt )


processResult : Result Json.Decode.Error WebSocket.WebSocketMsg -> Model -> Model
processResult result model =
    case result of
        Err decodeError ->
            Problem (PortMessageDecodeFailure decodeError)

        Ok wsMsg ->
            case wsMsg of
                WebSocket.Error { error } ->
                    Problem (UnexpectedMessage error)

                WebSocket.Data { data } ->
                    processMessage data model


processMessage : String -> Model -> Model
processMessage data model =
    case String.lines data of
        [ "connection established" ] ->
            Connected

        [ "connection failed" ] ->
            Problem ConnectionFailed

        "error" :: errorLines ->
            Problem (CompilationError (String.join "\n" errorLines))

        [ "success", hash ] ->
            Running hash

        [ "warning", warning ] ->
            case model of
                _ ->
                    Problem (CompilationError warning)

        _ ->
            Problem (UnexpectedMessage data)


view : Model -> Html Msg
view model =
    Html.div [ class "app" ]
        [ Html.div [ Attr.id "view" ]
            [ case model of
                Disconnected ->
                    Html.text "Disconnected"

                Connected ->
                    Html.text "Connected"

                Compiling ->
                    -- it would be nice to have some progress indication
                    -- (at least animated spinner or something)
                    Html.text "Compiling ..."

                Running hash ->
                    -- it would be nice to have some progress indication
                    -- (at least animated spinner or something)
                    -- Html.text ("Running: " ++ hash)
                    Html.iframe [ src (webBackend ++ "loader.html?hash=" ++ hash) ] []

                Problem problem ->
                    problemView problem
            ]
        ]


problemView : Problem -> Html msg
problemView problem =
    case problem of
        CompilationError error ->
            Html.div []
                [ Html.h1 [] [ Html.text "Compilation failed" ]
                , Html.pre [] [ Html.text error ]
                ]

        ConnectionFailed ->
            Html.div []
                [ Html.text "Failed to establish connection to server. Sorry. :-/"
                ]

        PortMessageDecodeFailure decodeError ->
            Html.text ("Failed to decode Port message. The error was: " ++ Json.Decode.errorToString decodeError)

        UnexpectedMessage problemDescription ->
            Html.text ("Unexpected message: " ++ problemDescription)
