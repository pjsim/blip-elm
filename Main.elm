module Main exposing (..)

import AnimationFrame
import Html exposing (Html, div, text)
import Key exposing (..)
import Keyboard exposing (KeyCode)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)
import Tuple
import Collision2D


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { velocity : Float
    , position : Float
    , shotsFired : Int
    , pingActivated : Bool
    , pingGrowth : Float
    , pingStrength : Float
    , seamineLocations : List ( Float, Float )
    , seamineDropSpeed : Float
    , gameOver : Bool
    }


model : Model
model =
    { velocity = 0
    , position = 300.0
    , shotsFired = 0
    , pingActivated = False
    , pingGrowth = 0.0
    , pingStrength = 0.0
    , seamineLocations = []
    , seamineDropSpeed = 1.0
    , gameOver = False
    }


init : ( Model, Cmd Msg )
init =
    ( model, Random.generate SpawnMine randomPoint )


randomPoint : Random.Generator ( Float, Float )
randomPoint =
    Random.pair (Random.float 50.0 550.0) (Random.float -200.0 0.0)



-- UPDATE


type Msg
    = Tick Time
    | KeyDown KeyCode
    | KeyUp KeyCode
    | SpawnMine ( Float, Float )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SpawnMine positions ->
            if List.length model.seamineLocations < 5 then
                ( { model | seamineLocations = model.seamineLocations ++ [ positions ] }, Random.generate SpawnMine randomPoint )
            else
                ( { model | seamineLocations = model.seamineLocations ++ [ positions ] }, Cmd.none )

        Tick dt ->
            -- ( applyPhysics dt model, Cmd.none )
            updateFrame dt model

        -- updateFrame model time
        KeyDown keyCode ->
            ( keyDown keyCode model, Cmd.none )

        KeyUp keyCode ->
            ( keyUp keyCode model, Cmd.none )


updateFrame : Float -> Model -> ( Model, Cmd Msg )
updateFrame dt model =
    let
        newModel =
            model
                |> pingFinished
                |> dissipatePing
                |> moveSub dt
                |> checkForCollision
                |> updateSeamineLocations
                |> clearOldMines
    in
        if List.length model.seamineLocations < 5 || round dt % 100 == 0 then
            ( newModel, Random.generate SpawnMine randomPoint )
        else
            ( newModel, Cmd.none )


updateSeamineLocations : Model -> Model
updateSeamineLocations model =
    let
        newSeaminLocations =
            List.map2 updateSeamineLocation model.seamineLocations (List.repeat (List.length model.seamineLocations) model.seamineDropSpeed)
    in
        { model | seamineLocations = newSeaminLocations }


updateSeamineLocation : ( Float, Float ) -> Float -> ( Float, Float )
updateSeamineLocation ( h, v1 ) v2 =
    ( h, v1 + v2 )


clearOldMines : Model -> Model
clearOldMines model =
    let
        newSeamines =
            List.filter checkSeamineInMap model.seamineLocations

        newModel =
            { model | seamineLocations = newSeamines }
    in
        newModel


formatSeamineLocation : ( Float, Float ) -> Float -> ( Float, Float )
formatSeamineLocation ( h, v1 ) v2 =
    ( h, v1 + v2 )


checkSeamineInMap : ( Float, Float ) -> Bool
checkSeamineInMap ( h, v ) =
    if v < 600 then
        True
    else
        False


checkForCollision : Model -> Model
checkForCollision model =
    if List.member True (seaminesCollisionCheck model) then
        { model | gameOver = True }
    else
        model


seamineCollisionCheck : Model -> ( Float, Float ) -> Bool
seamineCollisionCheck model ( horizontal, vertical ) =
    let
        sub_top =
            450

        sub_bottom =
            350

        sub_left =
            model.position - 10

        sub_right =
            model.position + 10

        seamine_top =
            vertical + model.seamineDropSpeed - 15

        seamine_bottom =
            vertical + model.seamineDropSpeed + 15

        seamine_left =
            horizontal - 15

        seamine_right =
            horizontal + 15

        submarineCircle =
            Collision2D.circle model.position 400 50

        seamineCircle =
            Collision2D.circle horizontal vertical 30
    in
        if
            Collision2D.circleToCircle submarineCircle seamineCircle
            -- ((sub_top < seamine_bottom && sub_bottom > seamine_bottom) || (sub_bottom > seamine_top && sub_bottom < seamine_bottom))
            --     && ((sub_right > seamine_left && seamine_right < seamine_right) || (sub_left < seamine_right && sub_left > seamine_left))
        then
            True
        else
            False


seaminesCollisionCheck : Model -> List Bool
seaminesCollisionCheck model =
    List.map2 seamineCollisionCheck (List.repeat (List.length model.seamineLocations) model) model.seamineLocations


moveSub : Float -> Model -> Model
moveSub dt model =
    { model | position = model.position + model.velocity * (dt / 5) }


dissipatePing : Model -> Model
dissipatePing model =
    if model.pingActivated == True then
        { model | pingGrowth = model.pingGrowth + 5, pingStrength = model.pingStrength - 0.02 }
    else
        model


pingFinished : Model -> Model
pingFinished model =
    if model.pingGrowth > 250 then
        { model | pingActivated = False, pingGrowth = 0, pingStrength = 0.0 }
    else
        model


keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    case Key.fromCode keyCode of
        Space ->
            -- incrementShotsFired model
            if model.pingActivated == False then
                { model
                    | pingActivated = True
                    , pingStrength = 1
                    , seamineDropSpeed = model.seamineDropSpeed + 1
                }
            else
                model

        ArrowLeft ->
            updateVelocity -1.0 model

        ArrowRight ->
            updateVelocity 1.0 model

        Restart ->
            if model.gameOver == True then
                Tuple.first init
            else
                model

        _ ->
            model


keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
    case Key.fromCode keyCode of
        ArrowLeft ->
            updateVelocity 0 model

        ArrowRight ->
            updateVelocity 0 model

        _ ->
            model


updateVelocity : Float -> Model -> Model
updateVelocity newVelocity model =
    { model | velocity = newVelocity }



-- VIEW


view : Model -> Html Msg
view model =
    let
        instructions =
            ("Instructions: Left, Right, Space to Ping."
                ++ if model.gameOver == True then
                    " Press R to restart."
                   else
                    ""
            )
    in
        div []
            [ div []
                -- [ Html.text (toString model) ]
                -- , div []
                [ Html.text instructions
                ]
            , svg
                [ width "650", height "650", viewBox "0 0 650 650" ]
                (displayElements model)
            ]


displayElements : Model -> List (Svg Msg)
displayElements model =
    if model.gameOver == True then
        [ gameOverBackground ]
    else
        elements model


elements : Model -> List (Svg Msg)
elements model =
    [ background, ping model ] ++ seamines model ++ submarine model


gameOverBackground : Svg Msg
gameOverBackground =
    rect
        [ x "10"
        , y "10"
        , width "600"
        , height "600"
        , rx "5"
        , ry "5"
        , fill "rgba(180, 0, 0, 1)"
        ]
        []


background : Svg Msg
background =
    rect
        [ x "10"
        , y "10"
        , width "600"
        , height "600"
        , rx "5"
        , ry "5"
        , fill "rgba(0, 0, 100, 1)"
        ]
        []


ping : Model -> Svg Msg
ping model =
    circle
        [ cx (toString model.position)
        , cy "400"
        , r (toString model.pingGrowth)
        , fill (String.concat [ "rgba(0, 0, 255, ", toString model.pingStrength, ")" ])
        ]
        []


submarine : Model -> List (Svg Msg)
submarine model =
    [ ellipse
        [ cx (toString model.position)
        , cy "400"
        , rx "25"
        , ry "50"
        , fill "rgba(250, 250, 0, 1)"
        ]
        []
    , circle
        [ cx (toString model.position)
        , cy "450"
        , r "20"
        , fill "rgba(250, 250, 0, 1)"
        ]
        []
    ]



-- speedLines : Model -> List (Svg Msg)
-- speedLines model =
--     List.map2 speedLine model.speedLinePositions (List.repeat (List.length model.speedLinePositions) model.movementSpeed)
-- speedLine : Float -> Float -> Svg Msg
-- speedLine position speed =
--     rect
--         [ x (toString position)
--         , y (toString speed)
--         , width "5"
--         , height "25"
--         , rx "5"
--         , ry "5"
--         , fill "rgba(0, 0, 255, 0.6)"
--         ]
-- []


seamines : Model -> List (Svg Msg)
seamines model =
    List.map2 seamine model.seamineLocations (List.repeat (List.length model.seamineLocations) model.pingStrength)


seamine : ( Float, Float ) -> Float -> Svg Msg
seamine ( horizontal, vertical ) visibility =
    circle
        [ cx (toString horizontal)
        , cy (toString vertical)
        , r "30"
        , fill ("rgba(100, 100, 100, " ++ toString visibility ++ ")")
        ]
        []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
