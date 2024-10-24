app
---

* [x] try-multi-rectangles.hs
    + [x] MAIN
    + [x] BODY
        - [x] body
        - [x] viewProj
    + [x] RECTANGLES
        - [x] rects1
        - [x] rects2
        - [x] calcModel
* [x] try-boxes.hs
    + [x] MAIN
        - [x] main
    + [x] SEND REQUEST/EVENT TO VULKAN/MOFFY
        - [x] mffReqsToVk
        - [x] vkEvToMoffy
    + [x] BOX TO RECTANGLE
        - [x] boxToRect
        - [x] colorToColor
        - [x] lookupOr
    + [x] RUN BOXES
        - [x] runBoxes
* [ ] try-followbox.hs

src
---

* [x] Rectangles
* [x] VulkanRectangles
* [x] UseCairo
    + [x] PRE
        - [x] textureWidth
        - [x] textureHeight
    + [x] RUN
        - [x] rectangles
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
        - [x] unfrmBffrOstAlgn
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
        - [x] createDscSt
        - [x] dscWrite
    + [x] PROVIDE WINDOW OBJECTS
        - [x] provideWinObjs
        - [x] initWin
    + [x] WINDOW OBJECTS
        - [x] winObjs
        - [x] destroyWinObjs
        - [x] class NumToVal
        - [x] instance NumToVal
        - [x] WinObjs
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
        - [x] recreateImgVws
        - [x] createRndrPss
        - [x] createFrmbffrs
        - [x] recreateFrmbffrs
        - [x] createSyncObjs
        - [x] SyncObjs
    + [x] RECTANGLE BUFFER
        - [x] createRctBffr
        - [x] destroyRctBffr
        - [x] RectGroups
    + [x] GET GLFW EVENTS
        - [x] setGlfwEvents
        - [x] glfwEvents
        - [x] getMouseButtons
        - [x] MouseButtonStateDict
        - [x] mouseButtonAll
        - [x] sendMouseButtonDown
        - [x] sendMouseButtonUp
        - [x] sendMouseButton
    + [x] CREATE AND COPY BUFFERS
        - [x] createBffrAtm
        - [x] createBffrLst
        - [x] createBffrLst'
        - [x] copyBffrLst
    + [x] GRAPHICS PIPELINE
        - [x] createGrPpl
        - [x] recreateGrPpl
        - [x] grPplInfo
        - [x] shaderStages
        - [x] GlslVertexShaderArgs
        - [x] GlslFragmentShaderArgs
        - [x] vwpSt
        - [x] clrBlnd
    + [x] MAINLOOP
        - [x] mainloop
        - [x] class Succable
        - [x] instance Succable Bool
        - [x] instance Succable Int
        - [x] Devices
        - [x] PipelineLayout
        - [x] VertexBuffers
        - [x] wobjsToWin
        - [x] getPangoLayoutExtent
        - [x] run
    + [x] RECREATE
        - [x] catchAndRecreate
        - [x] recreateAllIfNeed
        - [x] recreateAll
        - [x] waitFrmbffrSize
        - [x] Recrs
        - [x] wobjsToRecrs
    + [x] DARW
        - [x] draw
        - [x] catchAndSerialize
        - [x] Draws
        - [x] wobjsToDrs
        - [x] recordCmdBffr
        - [x] bffrLstLn
    + [x] DATA TYPES
        - [x] WVertex
        - [x] Vertex
        - [x] instance StrG.G Vertex
        - [x] TexCoord
        - [x] verrtices
        - [x] indices
        - [x] Rectangle
        - [x] WRect
        - [x] RectangleRaw
        - [x] rectToRectRaw
        - [x] instance StrG.G RectangleRaw
        - [x] RectPos
        - [x] RectSize
        - [x] RectColor
        - [x] RectModel
        - [x] RectModel0
        - [x] RectModel1
        - [x] RectModel2
        - [x] RectModel3
        - [x] dummy
        - [x] WViewProj
        - [x] ViewProjection
        - [x] viewProjIdentity
        - [x] instance StrG.G ViewProjection
        - [x] instance Default ViewProjection
    + [x] SHADERS
        - [x] glslVertexShader
        - [x] glslFragmentShader
* [x] CreateTextureGroup
* [x] DrawView
    + [x] DRAW VIEW
        - [x] drawView
        - [x] drRect
        - [x] drLn
        - [x] drTxt
        - [x] drImg
    + [x] CAIRO ARGB 32
        - [x] CairoArgb32
        - [x] instance BObj.IsImage CairoArgb32
        - [x] PixelRgba
        - [x] instance Storable (PixelRgba d)
        - [x] pixelArgb32ToRgba
        - [x] pixelRgbaToArgb32
* [ ] KeyToXKey
* [ ] KeyToXKey.TH
