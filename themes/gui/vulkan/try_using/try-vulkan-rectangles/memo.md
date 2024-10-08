app
---

* [x] try-cairo-image-as-texture.hs
* [x] try-use-cairo.hs
    + [x] main
    + [x] mainloop
    + [x] LR
    + [x] lr
    + [x] viewProj
    + [x] Angle
    + [x] rectangles
    + [x] import list
* [x] try-use-texture-group
    + [x] MAIN
        - [x] main
        - [x] realMain
        - [x] Angle
        - [x] controller
        - [x] setPicture
        - [x] imgDir
        - [x] mdlDir
    + [x] BODY
        - [x] body
        - [x] draw
        - [x] viewProj
        - [x] processEvent
        - [x] inputTxFilePath
        - [x] keyToChar
    + [x] RECTANGLES
        - [x] rectTable
        - [x] RectangleTable
        - [x] scaled
        - [x] rotrated
        - [x] rotated
        - [x] scale
        - [x] rotate
    + [x] import list
* [ ] try-multi-rectangles.hs
* [ ] try-multi-rectangles2.hs

src
---

* [x] Texture
    + [x] CREATE AND BIND IMAGE
    + [x] CREATE IMAGE BUFFER
    + [x] WRITE BUFFER
    + [x] COPY FROM BUFFER TO IMAGE
* [x] UseCairo
    + [x] USE CAIRO LOOP
    + [x] CONTROLLER
    + [x] WINDOW, INSTANCE AND PHYSICAL AND LOGICAL DEVICES
        - [x] createIst
        - [x] vldLayers
        - [x] dbgMsngrInfo
        - [x] createWin
        - [x] pickPhd
        - [x] dvcExtensions
        - [x] findQFams
        - [x] QFamIndices
        - [x] createLgDvc
    + [x] BODY
        - [x] body
        - [x] createCmdPl
        - [x] createCmdBffr
        - [x] unfrmBffrOstAlgn
        - [x] createPplLyt
        - [x] createDscStLyt
        - [x] DscStLytArg
        - [x] AtomViewProj
        - [x] createViewProjBffr
        - [x] ViewProjMemory
        - [x] createDscPl
        - [x] createDscSt
        - [x] creteVtxBffr
        - [x] createIdxBffr
        - [x] createBffrMem
        - [x] createTxSmplr
    + [x] WINDOW OBJECTS
        - [x] winObjs
        - [x] RectGroups
        - [x] WinObjs
        - [x] winEnvs
        - [x] FramebufferResized
        - [x] FramebufferResizedState
        - [x] checkResizedState
        - [x] Swapchains
        - [x] createSwpch
        - [x] querySwpchSupport
        - [x] SwpchSupportDetails
        - [x] chooseSwpSfcFmt
        - [x] recreateSwpch
        - [x] querySwpchSupportFmt
        - [x] SwpchSupportDetailsFmt
        - [x] swapExtent
        - [x] swpchInfo
        - [x] createImgVws
        - [x] recreateImgVws
        - [x] createRndrPss
        - [x] createFrmbffrs
        - [x] RecreateFrmbffrs
            * [x] class
            * [x] instance '[] '[]
            * [x] instance (si ': sis) (sf ': sfs)
        - [x] frmbffrInfo
        - [x] createSyncObjs
        - [x] SyncObjs
        - [x] createRectangleBuffer
        - [x] destroyRectangleBuffer
    + [x] CREATE GRAPHICS PIPELINE
        - [x] createGrPpl
        - [x] recreateGrPpl
        - [x] Pipeline
        - [x] grPplInfo
        - [x] shaderStages
        - [x] GlslVertexShaderArgs
        - [x] GlslFragmentShaderArgs
        - [x] vwpSt
        - [x] clrBlnd
    + [x] CREATE BUFFER
        - [x] createBffrAtm
        - [x] createBffrLst
        - [x] createBffrLst'
    + [x] MAIN LOOP AND RUN
        - [x] mainLoop
        - [x] Devices
        - [x] PipelineLayout
        - [x] VertexBuffers
        - [x] winObjsToWin
        - [x] run
    + [x] RECREATE
        - [x] catchAndRecreate
        - [x] Recreates
        - [x] winObjsToRecreates
        - [x] recreateAll
        - [x] waitFramebufferSize
    + [x] DRAW
        - [x] draw
        - [x] Draws
        - [x] winObjsToDraws
        - [x] recordCmdBffr
        - [x] bffrLstLn
        - [x] updateViewProjBffr
    + [x] DATA TYPES
        - [x] Rectangle
        - [x] rectToRectRaw
        - [x] WRect
        - [x] RectangleRaw
        - [x] dummyRect
        - [x] instance StrG.G RectangleRaw
        - [x] RectPos
        - [x] RedtSize
        - [x] RectColor
        - [x] RectModel
        - [x] RectModel0
        - [x] RectModel1
        - [x] RectModel2
        - [x] RectModel3
        - [x] WVertex
        - [x] Vertex
        - [x] instance StrG.G Vertex
        - [x] vertices
        - [x] indices
        - [x] WViewProj
        - [x] ViewProj
        - [x] instance StrG.G ViewProj
    + [x] SHADERS
        - [x] glslVertexShader
        - [x] glslFragmentShader
    + [x] export list
    + [x] import list
* [x] CairoImage
    + [x] drawViewIO
    + [x] CairoArgb32
    + [x] PixelRgba
    + [x] pixelArgb32ToPixelRgba
    + [x] pixelRgbaToPixelArgb32
    + [x] instance Storable (PixelRgba d)
    + [x] instance BObj.IsImage CairoArgb32
    + [x] drawView
    + [x] drawLine
    + [x] drawText
    + [x] drawImage
    + [x] import list
* [x] ConvertPixel
* [x] UseTextureGroup
    + [x] USE TEXTURE GROUP
        - [x] useTextureGroup
        - [x] Command
        - [x] Event
        - [x] createIst
        - [x] vldLayers
        - [x] dbgMsngrInfo
        - [x] pickPhd
        - [x] QFamIdcs
        - [x] findQFams
        - [x] dvcExtensions
        - [x] createLgDvc
        - [x] swpchImgNum
    + [x] BODY
        - [x] body
        - [x] createCmdPl
        - [x] createCmdBffr
        - [x] createPplLyt
        - [x] createDSLyt
        - [x] DscStLytArg
        - [x] createVtxBffr
        - [x] createIdxBffr
        - [x] createBffrMem
        - [x] createViewProjBffr
        - [x] ViewProjMemory
        - [x] AtomViewProj
        - [x] createDscPl
    + [x] PROVIDE WINDOW OBJECTS
        - [x] provideWinObjs
            * [x] forall ...
            * [x] constraint
            * [x] type declaration
            * [x] arguments
            * [x] body
        - [x] initWin
        - [x] createDscSt'
    + [x] WINDOW OBJECTS
        - [x] winObjs
        - [x] destroyWinObjs
        - [x] class NumToVal
        - [x] instance NumToVal
        - [x] WinObjs
            * [x] correct order of objects
            * [x] others
        - [x] WinEnvs
        - [x] FramebufferResized
        - [x] Swapchains
        - [x] Pipeline
        - [x] WinObjGroups
        - [x] prepareSwpch
        - [x] querySwpchSupport
        - [x] SwpchSupportDetails
        - [x] createSwpch'
        - [x] recreateSwpch
        - [x] swapExtent
        - [x] swpchInfoSsd
        - [x] chooseSwpSfcFmt
        - [x] swpchInfo
        - [x] createImgVws
        - [x] createImgVw
        - [x] recreateImageViews
        - [x] createRndrPss
        - [x] createFrmbffrs
        - [x] recreateFrmbffrs
        - [x] frmbffrInfo
        - [x] createSyncObjs
        - [x] SyncObjects
    * [x] RECTANGLE BUFFER
        - [x] createRctBffr
        - [x] destroyRctBffr
        - [x] RctGroups
    + [x] GET GLFW EVENTS
        - [x] setGlfwEvents
        - [x] glfwEvents
        - [x] getMouseButtons
        - [x] MouseButtonsStateDict
        - [x] mouseButtonAll
        - [x] sendMouseButtonDown
        - [x] sendMouseButtonUp
        - [x] sendMouseButton
    + [x] GRAPHICS PIPELINE
        - [x] createGrPpl
        - [x] recreateGrPpl
        - [x] grPplInfo
        - [x] shaderStages
        - [x] vwpSt
        - [x] clrBlnd
    + [x] CREATE AND COPY BUFFERS
        - [x] createBffrAtm
        - [x] createBffrLst
        - [x] createBffrLst'
        - [x] createBffr''
        - [x] copyBffrLst
    + [x] MAINLOOP
        - [x] mainloop
            * [x] forall
            * [x] constraint
            * [x] type declaration
            * [x] body
        - [x] class Succable
        - [x] instance Succable Bool
        - [x] instance Succable Int
        - [x] Devices
        - [x] PipelineLayout
        - [x] VertexBuffers
        - [x] winObjsToWin
        - [x] run
            * [x] forall
            * [x] constraint
            * [x] type declaration
            * [x] body
    + [x] RECREATE
        - [x] catchAndRecreate
            * [x] forall
            * [x] constraint
            * [x] type declaration
            * [x] body
        - [x] recreateAllIfNeed
            * [x] forall
            * [x] constraint
            * [x] type declaration
            * [x] body
        - [x] recreateAll
            * [x] forall
            * [x] constraint
            * [x] type declaration
            * [x] body
        - [x] waitFrmbffrSize
        - [x] Recreates
            * [x] order of type variables
            * [x] others
        - [x] wobjToRecrs
    + [x] DRAW
        - [x] draw
            * [x] forall
            * [x] constraint
            * [x] type declaration
            * [x] body
        - [x] catchAndSerialize
        - [x] updateViewProjBffr
        - [x] Draws
            * [x] order of arguments
            * [x] order of type variables
            * [x] others
        - [x] wobjsToDrs
        - [x] recordCommandBuffer
        - [x] bffrLstLn
    + [x] DATA TYPES
    + [x] SHADERS
    + [x] import list
    + [x] export list
* [x] CreateTextureGroup
    + [x] GROUP
        - [x] txGroup
        - [x] TextureGroup
    + [x] CREATE AND DESTROY
        - [x] createTx
        - [x] destroyTx
        - [x] createTxImg
        - [x] copyBffrToImg
    + [x] BUFFER
        - [x] createBffrImg
        - [x] createBffr
        - [x] createBffr'
        - [x] bffrInfo
        - [x] findMmType
        - [x] mmAllcInfo
    + [x] IMAGE
        - [x] createImg'
            * [x] forall
            * [x] constraint
            * [x] type declaration
            * [x] body
        - [x] transitionImgLyt
    + [x] IMAGE VIEW
    + [x] DESCRIPTOR SET
    + [x] BEGIN SINGLE TIME COMMANDS
    + [x] SAMPLER
    + [x] IMAGE TYPE
    + [x] import list
    + [x] export list
* [ ] Rectangles
* [ ] Rectangles2
* [ ] Vertex
* [ ] Tools
