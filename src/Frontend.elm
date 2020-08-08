module Frontend exposing (app, init, update, updateFromBackend, view)

import Ascii
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element
import Grid
import Html.Attributes
import Lamdera
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Point2d
import Task
import Types exposing (..)
import Url
import WebGL exposing (Shader)
import WebGL.Texture exposing (Texture)


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


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key, grid = Grid.empty, viewPoint = Point2d.origin, cursorPoint = Nothing, texture = Nothing }
    , WebGL.Texture.loadWith
        { magnify = WebGL.Texture.nearest
        , minify = WebGL.Texture.nearest
        , horizontalWrap = WebGL.Texture.clampToEdge
        , verticalWrap = WebGL.Texture.clampToEdge
        , flipY = False
        }
        Ascii.textureData
        |> Task.attempt TextureLoaded
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        TextureLoaded result ->
            case result of
                Ok texture ->
                    ( { model | texture = Just texture }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    ( model, Cmd.none )


view : FrontendModel -> Browser.Document msg
view model =
    { title = ""
    , body =
        [ Element.layout
            [ Element.width Element.fill, Element.height Element.fill ]
            (Element.html <|
                WebGL.toHtmlWith
                    [ WebGL.alpha False ]
                    [ Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "height" "100%"
                    ]
                    (case model.texture of
                        Just texture ->
                            [ WebGL.entityWith
                                []
                                vertexShader
                                fragmentShader
                                test
                                { view = Mat4.identity, texture = texture }
                            ]

                        Nothing ->
                            []
                    )
            )
        ]
    }


test =
    Grid.mesh (List.range 0 255 |> List.map (Ascii.ascii >> Maybe.withDefault Ascii.default))


type alias Uniforms =
    { view : Mat4
    , texture : Texture
    }


vertexShader : Shader { position : Vec2, texturePosition : Vec2 } { u | view : Mat4 } { vcoord : Vec2 }
vertexShader =
    [glsl|

attribute vec2 position;
attribute vec2 texturePosition;
uniform mat4 view;
varying vec2 vcoord;

void main () {
  gl_Position = view * vec4(position, 0.0, 1.0);
  vcoord = texturePosition;
}

|]


fragmentShader : Shader {} { u | texture : Texture } { vcoord : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vcoord;
        void main () {
            gl_FragColor = texture2D(texture, vcoord);
        }
    |]



--view : FrontendModel -> Browser.Document msg
--view model =
--    { title = ""
--    , body =
--        [ Element.layout
--            [ Element.Font.family [ Element.Font.monospace ], Element.Font.size 16, Element.moveDown 2 ]
--            (Ascii.ascii
--                |> List.Extra.groupsOf 25
--                |> List.map
--                    (List.map
--                        (String.fromChar
--                            >> Element.text
--                            >> Element.el [ Element.width (Element.px 10), Element.height (Element.px 16) ]
--                        )
--                        >> Element.row []
--                    )
--                |> Element.column [ Element.spacing 2, Element.htmlAttribute <| Html.Attributes.style "white-space" "pre" ]
--            )
--        ]
--    }
