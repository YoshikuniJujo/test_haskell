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
* [ ] try-use-texture-group
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
* [ ] UseTextureGroup
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
    + [ ] WINDOW OBJECTS
        - [x] winObjs
        - [x] destroyWinObjs
        - [x] class NumToVal
        - [x] instance NumToVal
        - [x] WinObjs
            * [x] correct order of objects
            * [x] others
        - [x] WinEnvs
        - [x] FramebufferResized
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
        - [ ] createFrmbffrs
        - [ ] recreateFramebuffers'
        - [ ] class Mappable
        - [ ] instance Mappable
        - [ ] mkFramebufferCreateInfo
        - [ ] createSyncObjs
        - [ ] SyncObjects
    * [ ] RECTANGLE BUFFER
    + [ ] GET GLFW EVENTS
        - [ ] setGlfwEvents
        - [ ] glfwEvents
        - [ ] getMouseButtons
        - [ ] MouseButtonsStateDict
        - [ ] mouseButtonAll
        - [ ] sendMouseButtonDown
        - [ ] sendMouseButtonUp
        - [ ] sendMouseButton
    + [ ] GRAPHICS PIPELINE
    + [ ] CREATE AND COPY BUFFERS
    + [ ] MAINLOOP
    + [ ] DATA TYPES
    + [ ] SHADERS
    + [ ] import list
    + [ ] export list
* [ ] CreateTextureGroup
* [ ] Vertex
* [ ] ThEnv
* [ ] Tools
* [ ] PangoLayoutExtent
* [ ] Rectangles
* [ ] Rectangles2
