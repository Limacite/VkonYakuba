module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Human =
    { name : String
    , sex : String
    , app : List String
    , img : Maybe String
    }


initHuman =
    { name = "名無しのVIP", sex = "", app = [], img = Nothing }


type alias Model =
    { page : Int
    , account : Human
    , husband : String
    , wife : String
    , husbImg : Maybe String
    , wifeImg : Maybe String
    , meetApp : Int
    , appSelect : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 initHuman "danna" "yome" Nothing Nothing 0 0, Cmd.none )



--UPDATE


type Msg
    = SelectPage Int
    | InputHusb String
    | InputWife String
    | Submit
    | ImgReqHusb
    | ImgSelHusb File
    | ImgLoadHusb String
    | ImgReqWife
    | ImgSelWife File
    | ImgLoadWife String
    | AppSelect Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectPage number ->
            ( { model | page = number }, Cmd.none )

        InputHusb input ->
            ( { model | husband = input }, Cmd.none )

        InputWife input ->
            ( { model | wife = input }, Cmd.none )

        Submit ->
            ( model, Cmd.none )

        ImgReqHusb ->
            ( model
            , Select.file [ "image/jpeg", "image/png" ] ImgSelHusb
            )

        ImgSelHusb img ->
            ( model, Task.perform ImgLoadHusb <| File.toUrl img )

        ImgLoadHusb content ->
            ( { model | husbImg = Just content }
            , Cmd.none
            )

        ImgReqWife ->
            ( model
            , Select.file [ "image/jpeg", "image/png" ] ImgSelWife
            )

        ImgSelWife img ->
            ( model, Task.perform ImgLoadWife <| File.toUrl img )

        ImgLoadWife content ->
            ( { model | wifeImg = Just content }
            , Cmd.none
            )

        AppSelect int ->
            ( { model | meetApp = int }, Cmd.none )



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        viewPage =
            case model.page of
                0 ->
                    viewHome

                1 ->
                    viewOther

                2 ->
                    viewFamily

                3 ->
                    setting

                _ ->
                    viewHome
    in
    div [ style "display" "flex", style "background-image" "url(img/bg_img.jpg) ", style "height" "100v", style "padding-bottom" "50px" ]
        [ div [ style "width" "20%", style "float" "left", hidden True ]
            [ ul [ style "list-style-type" "none" ]
                [ li [] [ label [ onClick (SelectPage 0) ] [ text "home" ] ]
                , li [] [ label [ onClick (SelectPage 1) ] [ text "他の家族" ] ]
                , li [] [ label [ onClick (SelectPage 2) ] [ text "家族を編集" ] ]
                , li [] [ label [ onClick (SelectPage 3) ] [ text "アカウント設定" ] ]
                ]
            ]
        , div [ style "width" "100%", style "margin" "0 auto" ]
            [ div [ style "font-size" "50px", style "text-align" "center", style "margin-top" "30px", style "margin-bottom" "30px" ] [ text "🏠Virtual役場🏠" ]
            , div [ style "height" "10px" ] [ hr [] [] ]
            , viewPage model
            ]
        ]



--- marry


viewHome : Model -> Html Msg
viewHome model =
    div [ style "border" "double medium #ff69b4", style "background-color" "#ffccfd", style "width" "172mm", style "height" "251mm", style "margin" "0 auto", style "padding" "40px" ]
        [ div [ style "font-size" "50px", style "color" "#ff00ff" ] [ text "婚姻届" ]
        , appSelect
        , br [] []
        , marryForm model
        ]


appSelect : Html Msg
appSelect =
    div [ style "margin-top" "25px", style "margin-bottom" "25px" ]
        [ div [ style "font-size" "20px" ] [ text "出会った場所" ]
        , br [] []
        , div [ style "margin-left" "20px" ]
            [ input [ type_ "radio", onClick (AppSelect 0) ] []
            , text "Reality"
            , input [ type_ "radio", onClick (AppSelect 1) ] []
            , text "Mirative"
            , input [ type_ "radio", onClick (AppSelect 2) ] []
            , text "IRIUM"
            , input [ type_ "radio", onClick (AppSelect 3) ] []
            , text "Twitter"
            ]
        ]


marryForm : Model -> Html Msg
marryForm model =
    Html.form [ onSubmit Submit, style "display" "flex", style "flex-direction" "column" ]
        [ div [ style "desplay" "flex" ]
            [ husbund model
            , wife model
            ]
        , br [] []
        , div [ style "text-align" "center" ]
            [ button
                [ style "font-size" "15px", style "font-weight" "bold", style "height" "2em", style "width" "50%", disabled (String.length model.husband < 1 && String.length model.wife < 1) ]
                [ text "宣誓" ]
            ]
        ]


husbund : Model -> Html Msg
husbund model =
    div [ style "width" "50%", style "float" "left", style "text-align" "center" ]
        [ div []
            [ label [ style "font-size" "20px" ] [ text "夫:" ]
            , textarea
                [ Html.Attributes.value model.husband
                , onInput InputHusb
                , style "font-size" "20px"
                , style "height" "1em"
                , style "resize" "none"
                , style "padding" "0px"
                , style "background-color" "transparent"
                ]
                []
            ]
        , br [] []
        , div []
            [ case model.husbImg of
                Nothing ->
                    div [ style "width" "90%", style "height" "50vw", style "background-color" "gray" ] []

                Just content ->
                    img [ src content, style "width" "90%", style "height" "50vw" ] []
            ]
        , label [ onClick ImgReqHusb, style "border" "solid 1px #000000" ] [ text "画像を選択" ]
        ]


wife : Model -> Html Msg
wife model =
    div [ style "width" "50%", style "float" "left", style "text-align" "center" ]
        [ div []
            [ label [ style "font-size" "20px" ] [ text "嫁:" ]
            , textarea
                [ Html.Attributes.value model.wife
                , onInput InputWife
                , style "font-size" "20px"
                , style "height" "1em"
                , style "resize" "none"
                , style "padding" "0px"
                , style "background-color" "transparent"
                ]
                []
            ]
        , br [] []
        , div []
            [ case model.wifeImg of
                Nothing ->
                    div [ style "width" "90%", style "height" "50vw", style "background-color" "gray" ] []

                Just content ->
                    img [ src content, style "width" "90%", style "height" "50vw", style "background-color" "gray" ] []
            ]
        , label [ onClick ImgReqWife, style "border" "solid 1px #000000" ] [ text "画像を選択" ]
        ]



---serch


viewOther : Model -> Html Msg
viewOther model =
    div []
        [ text "他のカップルを検索"
        , appSelect
        , cuppleList model.appSelect
        ]


cuppleList : Int -> Html Msg
cuppleList int =
    div [] []



--- editFamily


viewFamily : Model -> Html Msg
viewFamily model =
    div []
        [ text "家族を編集"
        ]



{-
   familyList : Model -> Html Msg
   familyList model =
       ul
        [ style "list-style-type" "none"
        , style "margin-left" "0px"
        , style "padding-left" "0px"
        , style "border-top" "solid 1px #000000"
        , style "width" "100%"
        , style "height" "400px"
        , style "overflow-y" "scroll" ]
        (List.map viewPostContent model.postList)

   viewFamily : Model -> Html Msg
   viewFamily model human =
       li [ style "border" "solid 1px #000000", style "border-top" "none" ]
           [ div [] [ text human.name ]
           , div [] [ img [ style "height" "150px", src human.img, hidden (String.length human.img < 1) ] [] ]
           , div [ style "text-align" "right" ] [ text human.app ]
           ]
-}
--- setting


setting : Model -> Html Msg
setting model =
    let
        icon =
            case model.account.img of
                Just src ->
                    src

                Nothing ->
                    "./img/defaultIcon.png"
    in
    div [ style "display" "flex", style "margin-top" "50px" ]
        [ img
            [ style "height" "150px", style "width" "150px", style "border-radius" "50%", style "border-position" "left top", src icon ]
            []
        , div [ style "margin-left" "50px" ]
            [ label [] [ text ("名前：" ++ model.account.name) ] ]
        ]
