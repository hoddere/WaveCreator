module SinCreator exposing (..)

{-
Emily Hodder - 400008260
Evan Kieft - 001315430

User: A young elementary school student

Activity: The user is creating an annimation for a class project. They want 
    their objects to be able to move, and are using the SinCreator 
    to help generate the motions.

Emotion: The user is excited about their activity of creating an annimation,
    but anxious and confused about how to use the SinCreator to annimate
    their objects as they have no knowledge of the mathematics of a sin wave,
    and very minimal knowledge of elm.

Tasks: This activity will involve the user creating and annimating objects.
    Each object will have it's own unique movements

Typical Interaction: A student will open the wave creator, wanting to determine
    the code they must add to their object in order to achieve the movement
    they would like. The user will already know what way they want their shape
    to move, but not how to do it or what thay action is called. To start, they
    will look at the apply transforms section and determine which of those options
    matches the motion they want to achieve. They will then select the option
    that fits their needs. They will then adjust the numbers of the sin function
    accordingly. Lastly, they will copy the movement code that has been generated.

Principle 1: Discoverability. Before the user had to click through all the 
    possible transformation options, but now they are all clearly displayed.
    This allows the user to quickly discover what all the possible
    transformations are and quickly and easily switch between them.

Principle 2: Conceptual Model. The other tabs of the shape creator all have
    a consistent format for their transformations. They have a section called
    "Apply Transforms" under which the user can simple click on the 
    transformation they want and it is automatically added to the shape.
    However, the wave creator did not follow this conceptual model that has
    been set out by the other tabs. We altered the design such that it now
    conforms to the conceptual model, making the application easier for the 
    user. 
    
-}

import Array exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import ShapeCreateAssets exposing (..)


init =
    { time = Nothing
    , currentTime = 0
    , notify = NotifyTap
    , uArg = 0
    , vArg = 0
    , editableArg = 0
    , uDilation = 1
    , vDilation = 1
    , editableDilation = 0
    , editableShift = 0
    , uScale = 5
    , vScale = 5
    , editableScale = 0
    , uShift = 0
    , uShiftScale = 1
    , u = 1
    , v = 1
    , rScale = 1
    , gScale = 1
    , bScale = 1
    , rFun = OneFun
    , bFun = UFun
    , gFun = VFun
    , sinGraph = []
    , cosGraph = []
    , vTransparency = 0.5
    , trigCycleU = Sin
    , trigCycleV = Sin
    , latestPointV = ( 0, 0, rgb 160 128 96 )
    , uTransform = ScaleU
    , moveX = ZeroFun
    , moveY = UFunZero
    , moveX1 = UFunZero
    , moveY1 = ZeroFun
    , transformFun = ZeroFun
    , uCosGraph = 0
    , uSinGraph = 0
    , editableYSinForTransforms = 0
    , r = 0
    , g = 0
    , b = 0
    , currentButton = None
    , buttonDownTime = 0
    , transformsRightArrowTransp = 0.25
    , transformsLeftArrowTransp = 0.25
    , moveTextX = 0.25
    , moveTextY = 0.25
    , moveTextX1 = 0.25
    , moveTextY1 = 0.25
    , rTransp = 0.25
    , gTransp = 0.25
    , bTransp = 0.25
    , addAnotherFuncTransp = 0.25
    , uTextTransp = 0.5
    , vTextTransp = 0.5
    , maxAmplitude = 20
    , maxFrequency = 10
    , maxShift = 2 * Basics.pi
    , cosWaveLength = 200
    , sinWaveLength = 100
    , hasScaleU = False
    , hasMoveX = False
    , hasMoveY = False
    , hasMoveCircle = False
    , hasURotate = False
    , hasScaleX = False
    , hasScaleY = False
    , hasMakeTransparent = False
    , hasEditableXSin = False
    }


type Msg m
    = Tick Float GetKeyState
    | TransM (m -> m)
    | Notif Notifications
    | R
    | G
    | B
    | UScalePlus
    | UDilationPlus
    | UShiftPlus
    | UScaleMinus
    | UDilationMinus
    | UShiftMinus
    | EditableScalePlus
    | EditableDilationPlus
    | EditableScaleMinus
    | EditableDilationMinus
    | VScalePlus
    | VScaleMinus
    | VDilationPlus
    | VDilationMinus
    | TrigCycleU
    | TrigCycleV
    | RScalePlus
    | RScaleMinus
    | GScalePlus
    | GScaleMinus
    | BScalePlus
    | BScaleMinus
    | ButtonDown ButtonDir
    | MouseUp
    | Toggle Transforms


type Notifications
    = NotifyTap
    | NotifyTapAt
    | NotifyEnter
    | NotifyEnterAt
    | NotifyLeave
    | NotifyLeaveAt
    | NotifyMouseMoveAt
    | NotifyMouseDown
    | NotifyMouseDownAt
    | NotifyMouseUp
    | NotifyMouseUpAt
    | NotifyTouchStart
    | NotifyTouchStartAt
    | NotifyTouchEnd
    | NotifyTouchEndAt
    | NotifyTouchMoveAt


type FunType
    = OneFun
    | UFun
    | VFun


type Trig
    = Sin
    | Cos


type ZeroFunType
    = ZeroFun
    | UFunZero
    | NegUFun
    | VFunZero
    | NegVFun


type Transforms
    = ScaleU
    | MoveX
    | MoveY
    | MoveCircle
    | URotate
    | ScaleX
    | ScaleY
    | MakeTransparent
    | EditableXSin


type ButtonDir
    = AmplitudeUp
    | AmplitudeDown
    | FrequencyUp
    | FrequencyDown
    | ShiftUp
    | ShiftDown
    | EditableAmplitudeUp
    | EditableAmplitudeDown
    | EditableFrequencyUp
    | EditableFrequencyDown
    | RedUp
    | RedDown
    | BlueUp
    | BlueDown
    | GreenUp
    | GreenDown
    | None
    | VUP
    | VDown


update msg model =
    case msg of
        Tick t _ ->
            let
                uArg =
                    model.uArg + model.uDilation * (t - (t - 0.05))

                vArg =
                    model.vArg + model.vDilation * (t - (t - 0.05))

                editableArg =
                    model.editableArg + model.editableDilation * (t - (t - 0.05))

                currentTime =
                    case model.time of
                        Nothing ->
                            0

                        Just ct ->
                            ct

                u =
                    model.uScale * evalTrig model.trigCycleU uArg

                v =
                    model.vScale * evalTrig model.trigCycleV vArg

                r =
                    clamp 0 255 (abs (model.rScale * eval model.rFun u v))

                g =
                    clamp 0 255 (abs (model.gScale * eval model.gFun u v))

                b =
                    clamp 0 255 (abs (model.bScale * eval model.bFun u v))

                uSinGraph =
                    model.uScale * sin uArg

                sinGraphPoint =
                    ( 0, uSinGraph, rgb r g b )

                cosGraphPoint =
                    ( uCosGraph, 0, rgb r g b )

                uCosGraph =
                    model.uScale * cos uArg

                editableYSinForTransforms =
                    model.editableScale * cos editableArg
            in
            { model
                | time = Just t
                , uArg = uArg
                , vArg = vArg
                , currentTime = currentTime
                , u = u
                , v = v
                , sinGraph =
                    List.take 2470
                        ([ sinGraphPoint ]
                            ++ List.filterMap
                                (\( xx, yy, cc ) ->
                                    if xx >= model.sinWaveLength then
                                        Nothing

                                    else
                                        Just ( xx + 0.35, yy, cc )
                                )
                                model.sinGraph
                        )
                , cosGraph =
                    List.take 2470
                        ([ cosGraphPoint ]
                            ++ List.filterMap
                                (\( xx, yy, cc ) ->
                                    -- Subtract 130 to account for the ratio of the screen and remove excess
                                    if yy <= -model.cosWaveLength then
                                        Nothing

                                    else
                                        Just ( xx, yy - 0.35, cc )
                                )
                                model.cosGraph
                        )
                , r = r
                , g = g
                , b = b
                , uCosGraph = uCosGraph
                , uSinGraph = uSinGraph

                --, editableYSinForTransforms = editableYSinForTransforms
                , buttonDownTime =
                    case model.currentButton of
                        None ->
                            0

                        _ ->
                            model.buttonDownTime + 0.1
                , uScale =
                    case model.currentButton of
                        AmplitudeUp ->
                            if model.uScale < model.maxAmplitude then
                                model.uScale + curveX model.buttonDownTime

                            else if model.uScale > model.maxAmplitude then
                                model.maxAmplitude

                            else
                                model.uScale

                        AmplitudeDown ->
                            if model.uScale > -model.maxAmplitude then
                                model.uScale - curveX model.buttonDownTime

                            else if model.uScale < -model.maxAmplitude then
                                -model.maxAmplitude

                            else
                                model.uScale

                        _ ->
                            model.uScale
                , uDilation =
                    case model.currentButton of
                        FrequencyUp ->
                            if model.uDilation < model.maxFrequency then
                                model.uDilation + curveX model.buttonDownTime

                            else if model.uDilation > model.maxFrequency then
                                model.maxFrequency

                            else
                                model.uDilation

                        FrequencyDown ->
                            if model.uDilation > -model.maxFrequency then
                                model.uDilation - curveX model.buttonDownTime

                            else if model.uDilation < -model.maxFrequency then
                                -model.maxFrequency

                            else
                                model.uDilation

                        _ ->
                            model.uDilation
                , uShift =
                    case model.currentButton of
                        ShiftUp ->
                            if model.uShift <= model.maxShift then
                                model.uShift + curveX model.buttonDownTime

                            else if model.uShift >= model.maxFrequency then
                                model.maxShift

                            else
                                model.uShift

                        ShiftDown ->
                            if model.uShift > -model.maxShift then
                                model.uShift - curveX model.buttonDownTime

                            else if model.uShift > model.maxFrequency then
                                -model.maxShift

                            else
                                model.uShift

                        _ ->
                            model.uShift
                , editableScale =
                    case model.currentButton of
                        EditableAmplitudeUp ->
                            if model.editableScale < model.maxAmplitude then
                                model.editableScale + curveX model.buttonDownTime

                            else if model.editableScale > model.maxAmplitude then
                                model.maxAmplitude

                            else
                                model.editableScale

                        EditableAmplitudeDown ->
                            if model.editableScale > -model.maxAmplitude then
                                model.editableScale - curveX model.buttonDownTime

                            else if model.editableScale < -model.maxAmplitude then
                                -model.maxAmplitude

                            else
                                model.editableScale

                        _ ->
                            model.editableScale
                , editableDilation =
                    case model.currentButton of
                        EditableFrequencyUp ->
                            if model.editableDilation < model.maxFrequency then
                                model.editableDilation + curveX model.buttonDownTime

                            else if model.editableDilation > model.maxFrequency then
                                model.maxFrequency

                            else
                                model.editableDilation

                        EditableFrequencyDown ->
                            if model.editableDilation > -model.maxFrequency then
                                model.editableDilation - curveX model.buttonDownTime

                            else if model.editableDilation < -model.maxFrequency then
                                -model.maxFrequency

                            else
                                model.editableDilation

                        _ ->
                            model.editableDilation
                , editableShift =
                    case model.currentButton of
                        ShiftUp ->
                            model.editableShift + curveX model.buttonDownTime

                        ShiftDown ->
                            model.editableShift - curveX model.buttonDownTime

                        _ ->
                            model.editableShift
                , rScale =
                    case model.currentButton of
                        RedUp ->
                            if model.rScale < 253 then
                                model.rScale + curveX model.buttonDownTime

                            else
                                model.rScale

                        RedDown ->
                            if model.rScale > 2 then
                                model.rScale - curveX model.buttonDownTime

                            else
                                model.rScale

                        _ ->
                            model.rScale
                , bScale =
                    case model.currentButton of
                        BlueUp ->
                            if model.bScale < 253 then
                                model.bScale + curveX model.buttonDownTime

                            else
                                model.bScale

                        BlueDown ->
                            if model.bScale > 2 then
                                model.bScale - curveX model.buttonDownTime

                            else
                                model.bScale

                        _ ->
                            model.bScale
                , gScale =
                    case model.currentButton of
                        GreenUp ->
                            if model.gScale < 252 then
                                model.gScale + curveX model.buttonDownTime

                            else
                                model.gScale

                        GreenDown ->
                            if model.gScale > 2 then
                                model.gScale - curveX model.buttonDownTime

                            else
                                model.gScale

                        _ ->
                            model.gScale
                , vScale =
                    case model.currentButton of
                        VUP ->
                            if model.vScale < 48 then
                                model.vScale + curveX model.buttonDownTime

                            else
                                model.vScale

                        VDown ->
                            if model.vScale > -48 then
                                model.vScale - curveX model.buttonDownTime

                            else
                                model.vScale

                        _ ->
                            model.vScale
            }

        TransM t ->
            t model

        Notif notif ->
            { model | notify = notif }

        R ->
            { model | rFun = cycleFun model.rFun }

        G ->
            { model | gFun = cycleFun model.gFun }

        B ->
            { model | bFun = cycleFun model.bFun }

        RScalePlus ->
            { model
                | rScale =
                    if model.rScale < 255 then
                        model.rScale + 1

                    else
                        model.rScale
            }

        RScaleMinus ->
            { model
                | rScale =
                    if model.rScale > 0 then
                        model.rScale - 1

                    else
                        model.rScale
            }

        GScalePlus ->
            { model
                | gScale =
                    if model.gScale < 255 then
                        model.gScale + 1

                    else
                        model.gScale
            }

        GScaleMinus ->
            { model
                | gScale =
                    if model.gScale > 0 then
                        model.gScale - 1

                    else
                        model.gScale
            }

        BScalePlus ->
            { model
                | bScale =
                    if model.bScale < 255 then
                        model.bScale + 1

                    else
                        model.bScale
            }

        BScaleMinus ->
            { model
                | bScale =
                    if model.bScale > 0 then
                        model.bScale - 1

                    else
                        model.bScale
            }

        UScalePlus ->
            { model
                | uScale =
                    if model.uScale < model.maxAmplitude then
                        model.uScale + 1

                    else
                        model.uScale
            }

        UScaleMinus ->
            { model
                | uScale =
                    if model.uScale > -model.maxAmplitude then
                        model.uScale - 1

                    else
                        model.uScale
            }

        UDilationPlus ->
            { model
                | uDilation =
                    if model.uDilation < model.maxFrequency then
                        model.uDilation + 1

                    else
                        model.uDilation
            }

        UDilationMinus ->
            { model
                | uDilation =
                    if model.uDilation > 0 then
                        model.uDilation - 1

                    else
                        model.uDilation
            }

        UShiftPlus ->
            { model
                | uArg =
                    if model.uShift < model.maxShift then
                        model.uArg + model.uShiftScale * Basics.pi / 4
                    else
                        0
                , uShift = 
                    if model.uShift < 7 then
                        model.uShift + model.uShiftScale
                    else
                        0
            }
        
        UShiftMinus ->
            { model
                | uArg =
                    if model.uShift > -model.maxShift then
                        model.uArg - model.uShiftScale * Basics.pi / 4
                    else
                        0
                , uShift = 
                    if model.uShift > -7 then
                        model.uShift - model.uShiftScale
                    else
                        0
            }

        EditableScalePlus ->
            { model
                | editableScale =
                    if model.editableScale < model.maxAmplitude then
                        model.editableScale + 1

                    else
                        model.editableScale
            }

        EditableScaleMinus ->
            { model
                | editableScale =
                    if model.editableScale > -model.maxAmplitude then
                        model.editableScale - 1

                    else
                        model.editableScale
            }

        EditableDilationPlus ->
            { model
                | editableDilation =
                    if model.editableDilation < model.maxFrequency then
                        model.editableDilation + 1

                    else
                        model.editableDilation
            }

        EditableDilationMinus ->
            { model
                | editableDilation =
                    if model.editableDilation > -model.maxFrequency then
                        model.editableDilation - 1

                    else
                        model.editableDilation
            }

        VScalePlus ->
            { model
                | vScale =
                    if model.vScale < model.maxAmplitude then
                        model.vScale + 1

                    else
                        model.vScale
            }

        VScaleMinus ->
            { model
                | vScale =
                    if model.vScale > -model.maxAmplitude then
                        model.vScale - 1

                    else
                        model.vScale
            }

        VDilationPlus ->
            { model
                | vDilation =
                    if model.vDilation < model.maxFrequency then
                        model.vDilation + 1

                    else
                        model.vDilation
            }

        VDilationMinus ->
            { model | vDilation = model.vDilation - 1 }

        TrigCycleU ->
            { model | trigCycleU = cycleTrig model.trigCycleU }

        TrigCycleV ->
            { model | trigCycleV = cycleTrig model.trigCycleV }

        ButtonDown dir ->
            { model | currentButton = dir }

        MouseUp ->
            { model | currentButton = None }

        Toggle MoveX ->
            { model | hasMoveX = not model.hasMoveX }

        Toggle MoveY ->
            { model | hasMoveY = not model.hasMoveY }

        Toggle URotate ->
            { model | hasURotate = not model.hasURotate }

        Toggle ScaleU ->
            { model | hasScaleU = not model.hasScaleU }

        Toggle ScaleX ->
            { model | hasScaleX = not model.hasScaleX }

        Toggle ScaleY ->
            { model | hasScaleY = not model.hasScaleY }

        Toggle MakeTransparent ->
            { model | hasMakeTransparent = not model.hasMakeTransparent }

        Toggle MoveCircle ->
            { model | hasMoveCircle = not model.hasMoveCircle }

        Toggle EditableXSin ->
            { model | hasEditableXSin = not model.hasEditableXSin }



transforms model =
    group
        [ rect 140 70 |> filled (rgba 255 255 255 0.5) |> addOutline (solid 1) lightGrey |> move ( -35, -21 )
        , rect 95 12 |> filled white |> addOutline (solid 1) lightGrey |> move ( -45, 14 )
        , text "Apply Transforms! (Pick one)" |> serif |> italic |> size 10 |> filled titleColour |> move ( -85, 11 )
        , group <|
            List.map2
                (\ss y ->
                    transformString model ss
                        |> text
                        |> fixedwidth
                        |> size 10
                        |> filled black
                        |> notifyTap (Toggle ss)
                        |> move ( -85, 11 )
                        |> time4 model ss 140 10
                        |> move ( -85, y )
                )
                [ ScaleU, MoveX, MoveY, MoveCircle, URotate, ScaleX, ScaleY, MakeTransparent, EditableXSin ]
                (List.map (\x -> -10 * Basics.toFloat x) (List.range 0 20))
        ]

transformString m t =
    case t of
        MoveX ->
            "Move along X"
            
        MoveY ->
            "Move along Y"

        MoveCircle ->
            "Move in a Circle"

        URotate ->
            "Rotate"

        ScaleU ->
            "Scale"

        ScaleX ->
            "Stretch in X direction"

        ScaleY ->
            "Stretch in Y direction"

        MakeTransparent ->
            "Blink (MakeTransparent)"
        
        EditableXSin ->
            "Move in X direction (editable)"
            
-- make the Collage fit in VGA screen minus menu bars, for Chromebooks and iPads


eval f u v =
    case f of
        OneFun ->
            u

        UFun ->
            u

        VFun ->
            v


showFun f u v =
    case f of
        OneFun ->
            "u"

        UFun ->
            "u"

        VFun ->
            "v"

time4 model t w h shape =
    if
        case t of
            MoveX ->
                model.hasMoveX
                
            MoveY ->
                model.hasMoveY
                
            MoveCircle ->
                model.hasMoveCircle

            URotate ->
                model.hasURotate

            ScaleX ->
                model.hasScaleX
                
            ScaleY ->
                model.hasScaleY
                
            ScaleU ->
                model.hasScaleU

            EditableXSin ->
                model.hasEditableXSin

            MakeTransparent ->
                model.hasMakeTransparent
    then
        group [ rect w h |> filled (rgba 255 137 5 (0.6 + 0.4 * sin (5 * model.time - 1.5))), shape ]

    else
        shape

cycleFun f =
    case f of
        OneFun ->
            UFun

        UFun ->
            VFun

        VFun ->
            OneFun


cycleTrig f =
    case f of
        Sin ->
            Cos

        Cos ->
            Sin


textTrig f =
    case f of
        Sin ->
            "sin"

        Cos ->
            "cos"


evalTrig f u =
    case f of
        Sin ->
            sin u

        Cos ->
            cos u


cycleFunZero f =
    case f of
        ZeroFun ->
            UFunZero

        UFunZero ->
            NegUFun

        NegUFun ->
            VFunZero

        VFunZero ->
            NegVFun

        NegVFun ->
            ZeroFun


moveText mv =
    case mv of
        ZeroFun ->
            "u"

        UFunZero ->
            "u"

        NegUFun ->
            "-u"

        VFunZero ->
            "v"

        NegVFun ->
            "-v"


applyTransforms tr model =
    let
        u =
            model.u
    in
    case tr of
        ScaleU ->
            scale ((model.uSinGraph + model.uScale) / 10)

        MoveX ->
            move ( model.uCosGraph, 0 )

        MoveY ->
            move ( 0, model.uSinGraph )

        MoveCircle ->
            move ( model.uCosGraph, model.uSinGraph )

        URotate ->
            rotate (u / 10)

        ScaleX ->
            scaleX ((model.uCosGraph + model.uScale) / 10)

        ScaleY ->
            scaleY ((model.uSinGraph + model.uScale) / 10)

        MakeTransparent ->
            makeTransparent u

        EditableXSin ->
            move ( model.uCosGraph, 0 )


applyTransformsText tr =
    case tr of
        MoveX ->
            " move x "

        MoveY ->
            " move y "

        MoveCircle ->
            " move in a circle "

        ScaleU ->
            " scale "

        URotate ->
            " rotate "

        ScaleX ->
            " scaleX "

        ScaleY ->
            " scaleY "

        MakeTransparent ->
            " makeTransparent "

        EditableXSin ->
            " editable Y Sin "


applyTransformsYourCode model tr =
    case tr of
        MoveX ->
            "|> move (" ++ String.fromFloat model.uScale ++ "*cos(model.time) , 0)"

        MoveY ->
            "|> move (0 , " ++ String.fromFloat model.uScale ++ "*sin(model.time))"

        MoveCircle ->
            "|> move (" ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model ++ ", " ++ String.fromFloat model.uScale ++ "*cos(" ++ cosinString model

        ScaleU ->
            "|> scale " ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model

        URotate ->
            "|> rotate " ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model

        ScaleX ->
            "|> scaleX " ++ String.fromFloat model.uScale ++ "*cos(" ++ cosinString model

        ScaleY ->
            "|> scaleY " ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model

        MakeTransparent ->
            "|> makeTransparent " ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model

        EditableXSin ->
            "|> move (" ++ String.fromFloat model.editableScale ++ "*cos(model.time) , " ++ "0" ++ ")"



-- change you app's state based on your new messages


numGraphPoints model =
    round 2505


curveX x =
    Basics.toFloat (round (clamp 0 12 (x ^ 2) / 4))


sinCurve model =
    let
        points =
            List.map2 (\x y -> ( x, y )) model.sinGraph (List.drop 1 model.sinGraph)
    in
    List.take (numGraphPoints model) (List.map (\( ( a, b, col1 ), ( c, d, col2 ) ) -> line ( a, b ) ( c, d ) |> outlined (solid 1) col1) points)


cosCurve model =
    let
        points =
            List.map2 (\x y -> ( x, y )) model.cosGraph (List.drop 1 model.cosGraph)
    in
    List.take (numGraphPoints model - 1) (List.map (\( ( a, b, col1 ), ( c, d, col2 ) ) -> line ( a, b ) ( c, d ) |> outlined (solid 1) col1) points)


cosinString model =
    let
        fraction =
            if (model.uShift / 8 * 2) < 0 then
                showDigits 5 (model.uShift / 8 * 2)

            else
                "+" ++ showDigits 4 (model.uShift / 8 * 2)
    in
    showDigits 2 model.uDilation ++ "*model.time" ++ fraction ++ "*Pi)"


view model =
    let
        uScale =
            model.uScale

        u =
            model.u

        v =
            model.v

        uArg =
            model.uArg

        x1 =
            if model.uTransform == MakeTransparent then
                90

            else
                45

        notTrigCycleU =
            if model.trigCycleU == Sin then
                cos

            else
                sin

        tt str =
            str |> text |> serif |> italic |> size 10 |> filled titleColour

        x2 =
            if model.uTransform == MakeTransparent then
                116

            else
                81

        yourCodeGroup =
            group
                [ rect 200 100 |> outlined (solid 1) red |> makeTransparent 0.25 |> move ( 100, 20 )
                , copiable "--Add these new definitions to your code" |> move ( 0, 62 )
                , copiable ("u = " ++ String.fromFloat model.uScale ++ "*" ++ textTrig model.trigCycleU ++ "(" ++ String.fromFloat model.uDilation ++ "*model.time+" ++ String.fromFloat model.uShift ++ ")") |> move ( 0, 52 )
                , copiable "mySquare = square 15" |> move ( 0, 32 )
                , copiable ("  |> outlined (solid 0.25) rgb (" ++ String.fromFloat model.rScale ++ "*" ++ showFun model.rFun u v ++ " " ++ String.fromFloat model.gScale ++ "*" ++ showFun model.gFun u v ++ " " ++ String.fromFloat model.bScale ++ "*" ++ showFun model.bFun u v ++ ")") |> move ( 35, 22 )
                , copiable ("  " ++ applyTransformsYourCode model model.uTransform) |> move ( 35, 12 )
                , copiable ("  |> move(" ++ moveText model.moveX1 ++ "," ++ moveText model.moveY1 ++ ")") |> move ( 35, 2 )
                , copiable "--Add the following code to your shapes:" |> move ( 0, -8 )
                , copiable "mySquare" |> move ( 10, -18 )
                ]

        transformsGraphicsGroup =
            group
                [ rect 210 200 |> outlined (solid 1) red |> makeTransparent 0.25 |> move ( -25, 70 )
                , square 15 |> outlined (solid 1) (rgb model.r model.g model.b) |> applyTransforms model.uTransform model |> move ( -25, 70 )
                , text (applyTransformsText ScaleU) |> size 10 |> filled black |> move ( 4, 105 )
                , text (applyTransformsText URotate) |> size 10 |> filled black |> move ( 30, 105 )
                --, group
                    --[ transforms--text (applyTransformsText ScaleU) |> size 10 |> filled black |> move ( 4, 105 )
                      --, text (applyTransformsText URotate) |> size 10 |> filled black |> move ( 4, 105 )
                --    , triangle 8 |> filled (rgb 255 10 10) |> rotate (degrees 180) |> notifyTap UTransformsReverse |> move ( -70, 105 ) |> notifyLeave (TransM (\m -> { m | transformsLeftArrowTransp = 0.25 })) |> notifyEnter (TransM (\m -> { m | transformsLeftArrowTransp = 1 })) |> makeTransparent model.transformsLeftArrowTransp
                --    , triangle 8 |> filled (rgb 255 10 10) |> notifyTap UTransforms |> move ( 100, 105 ) |> notifyLeave (TransM (\m -> { m | transformsRightArrowTransp = 0.25 })) |> notifyEnter (TransM (\m -> { m | transformsRightArrowTransp = 1 })) |> makeTransparent model.transformsRightArrowTransp
                --, text (moveText model.transformFun) |> size 10 |> filled black |> notifyTap TransformsFunctionChange |> move ( x1, 105 ) |> notifyLeave (TransM (\m -> { m | transformsNumTransp = 0.25 })) |> notifyEnter (TransM (\m -> { m | transformsNumTransp = 1 })) |> makeTransparent model.transformsNumTransp
                    --]
                    --|> move ( 30, 50 )
                ]

        setofTriangles =
            group
                [ upArrow |> notifyTap UDilationPlus |> move ( -67, -5 ) |> notifyMouseDown (ButtonDown FrequencyUp) |> notifyMouseUp (ButtonDown None)
                , upArrow |> notifyTap UScalePlus |> move ( -111, -5 ) |> notifyMouseDown (ButtonDown AmplitudeUp) |> notifyMouseUp (ButtonDown None)
                , upArrow |> notifyTap UShiftPlus |> move ( 17, -5 ) |> notifyMouseDown (ButtonDown ShiftUp) |> notifyMouseUp (ButtonDown None)
                , downArrow |> notifyTap UDilationMinus |> move ( -67, -20 ) |> notifyMouseDown (ButtonDown FrequencyDown) |> notifyMouseUp (ButtonDown None)
                , downArrow |> notifyTap UScaleMinus |> move ( -111, -20 ) |> notifyMouseDown (ButtonDown AmplitudeDown) |> notifyMouseUp (ButtonDown None)
                , downArrow |> notifyTap UShiftMinus |> move ( 17, -20 ) |> notifyMouseDown (ButtonDown ShiftDown) |> notifyMouseUp (ButtonDown None)
                ]

        -- Circle that rotates in time with the sin & cosin waves
        circleGraphics =
            group
                [ line ( -50, 50 ) ( -50 + model.uScale * notTrigCycleU uArg, 50 + u ) |> outlined (solid 1) (rgb model.r model.g model.b) |> makeTransparent 0.25
                , line ( -50 + model.uScale * notTrigCycleU uArg, 50 + u ) ( 0, 50 + model.uSinGraph ) |> outlined (solid 1) (rgb model.r model.g model.b) |> makeTransparent 0.5
                , line ( -50 + model.uScale * notTrigCycleU uArg, 50 + u ) ( model.uCosGraph - 50, 0 ) |> outlined (solid 1) (rgb model.r model.g model.b) |> makeTransparent 0.5
                , circle 2 |> filled (rgb model.r model.g model.b) |> move ( 0, 50 + model.uSinGraph )
                , circle 2 |> filled (rgb model.r model.g model.b) |> move ( model.uCosGraph - 50, 0 )
                , circle (abs uScale) |> outlined (solid 1) black |> move ( -50, 50 )
                , circle 2 |> filled (rgb model.r model.g model.b) |> move ( -50 + model.uScale * notTrigCycleU uArg, 50 + u )
                ]

        --transformTitles = transforms --tt "Apply Transforms (pick one)" |> move ( 55, 30 )

        cosLabel =
            text (String.fromFloat model.uScale ++ "* cos(" ++ cosinString model) |> fixedwidth |> size 8 |> filled black |> rotate (degrees 90) |> move ( -105, -100 ) |> notifyTap (TransM (\m -> { m | trigCycleU = Cos }))
    in
    [ graphPaperCustom 10 1 (rgb 10 10 10) |> makeTransparent 0.1 -- axes and selected coordinate ticks
    , group
        [ rect 1000 0.5 |> filled brown
        , rect 0.5 1000 |> filled brown
        , group (sinCurve model) |> move ( 0, 50 )
        , group (cosCurve model) |> move ( -50, 0 )
        , trigGraphAxis model |> move ( -185, 70 )
        , circleGraphics
        ]
        |> move ( -140, 70 )
    , cosLabel |> move ( -127, 20 )
    , transformsGraphicsGroup |> move ( 0, -110 )
    , group
        [ functionText model |> move ( 5, 150 )
        , setofTriangles |> move ( 0, 165 )
        ]
        |> move ( -20, 15 )
    --, transforms model |> move ( -150, 30 )
    , yourCodeGroup |> move ( 40, 110 )
    ]



upArrow =
    polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled (rgba 128 10 10 0.6)


downArrow =
    polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled (rgba 128 10 10 0.6)


trigGraphAxis model =
    group
        [ rect 0.5 105 |> filled black |> move ( 185, -18 )
        , rect model.sinWaveLength 0.5 |> filled black |> move ( 185 + model.sinWaveLength / 2, -20 )

        -- Subtract 130 to account for the ratio of the screen and remove excess
        , rect 105 0.5 |> filled black |> move ( 132, -70 )
        , rect 0.5 model.cosWaveLength |> filled black |> move ( 135, -70 - model.cosWaveLength / 2 )
        ]


functionText model =
    group
        [ text (showDigits 2 model.uScale ++ "*" ++ textTrig model.trigCycleU ++ "(" ++ cosinString model) |> fixedwidth |> size 10 |> filled black |> move ( -120, 0 )
        ]


showDigits width x =
    "      " ++ String.fromFloat x |> String.right width


titleColour =
    rgba 200 0 0 0.95


copiable str =
    str |> text |> selectable |> fixedwidth |> size 6 |> filled black


copiable2 str =
    str |> text |> selectable |> fixedwidth |> size 5 |> filled black
