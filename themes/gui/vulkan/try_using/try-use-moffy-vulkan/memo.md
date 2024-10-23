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
* [ ] UseCairo
    + [ ] PRE
        - [ ] textureSize
        - [ ] textureWidth
        - [ ] textureHeight
    + [ ] RUN
        - [x] rectangles
        - [ ] Command
        - [ ] Event
        - [x] createIst
        - [x] vldLayers
        - [x] dbgMsngrInfo
        - [x] pickPhd
        - [x] QFamIdcs
        - [x] findQFams
        - [x] dvcExtensions
        - [x] createLgDvc
        - [x] swpchImgNum
    + [ ] BODY
        - [ ] body
        - [x] createCmdPl
        - [x] createCmdBffr
        - [x] unfrmBffrOstAlgn
        - [x] createPplLyt
        - [x] createDSLyt
        - [x] DscStLytArg
        - [x] createVtxBffr
        - [x] createIdxBffr
        - [x] createBffrMem
        - [ ] createViewProjBffr
        - [x] ViewProjMemory
        - [x] AtomViewProj
        - [x] createDscPl
        - [ ] createDscSt
        - [ ] descriptorWrite
    + [ ] PROVIDE WINDOW OBJECTS
        - [ ] provideWinObjs
        - [ ] initWin
    + [ ] WINDOW OBJECTS
        - [ ] winObjs
        - [ ] destroyWinObjs
        - [ ] class NumToVal
        - [ ] instance NumToVal
        - [ ] WinObjs
        - [ ] WinEnvs
        - [ ] FramebufferResized
        - [ ] Swapchains
        - [ ] Pipeline
        - [ ] WinObjGroups
        - [ ] prepareSwpch
        - [ ] querySwpchSupport
        - [ ] SwpchSupportDetails
        - [ ] createSwpch'
        - [ ] recreateSwpch
        - [ ] swapExtent
        - [ ] swpchInfoSsd
        - [ ] chooseSwpSfcFmt
        - [ ] swpchInfo
        - [ ] createImgVws
        - [ ] recreateImgVws
        - [ ] createRndrPss
        - [ ] createFrmbffrs
        - [ ] recreateFrmbffrs
        - [ ] createSyncObjs
        - [ ] SyncObjs
    + [ ] RECTANGLE BUFFER
        - [ ] createRctBffr
        - [ ] destroyRctBffr
        - [ ] RectGroups
    + [ ] GET GLFW EVENTS
        - [ ] setGlfwEvents
        - [ ] glfwEvents
        - [ ] getMouseButtons
        - [ ] MouseButtonStateDict
        - [ ] mouseButtonAll
        - [ ] sendMouseButtonDown
        - [ ] sendMouseButtonUp
        - [ ] sendMouseButton
    + [ ] CREATE AND COPY BUFFERS
        - [ ] createBffrAtm
        - [ ] createBffrLst
        - [ ] createBffrLst'
        - [ ] copyBffrLst
    + [ ] GRAPHICS PIPELINE
        - [ ] createGrPpl
        - [ ] recreateGrPpl
        - [ ] grPplInfo
        - [ ] shaderStages
        - [ ] GlslVertexShaderArgs
        - [ ] GlslFragmentShaderArgs
        - [ ] vwpSt
        - [ ] clrBlnd
    + [ ] MAINLOOP
        - [ ] mainloop
        - [ ] class Succable
        - [ ] instance Succable Bool
        - [ ] instance Succable Int
        - [ ] Devices
        - [ ] PipelineLayout
        - [ ] VertexBuffers
        - [ ] wobjsToWin
        - [ ] run
    + [ ] RECREATE
        - [ ] catchAndRecreate
        - [ ] recreateAllIfNeed
        - [ ] recreateAll
        - [ ] waitFrmbffrSize
        - [ ] Recrs
        - [ ] wobjsToRecrs
    + [ ] DARW
        - [ ] draw
        - [ ] catchAndSerialize
        - [ ] Draws
        - [ ] wobjsToDrs
        - [ ] recordCmdBffr
        - [ ] bffrLstLn
    + [ ] DATA TYPES
        - [ ] WVertex
        - [ ] Vertex
        - [ ] instance StrG.G Vertex
        - [ ] TexCoord
        - [ ] verrtices
        - [ ] indices
        - [ ] Rectangle
        - [ ] WRect
        - [ ] RectangleRaw
        - [ ] rectToRectRaw
        - [ ] instance StrG.G RectangleRaw
        - [ ] RectPos
        - [ ] RectSize
        - [ ] RectColor
        - [ ] RectModel
        - [ ] RectModel0
        - [ ] RectModel1
        - [ ] RectModel2
        - [ ] RectModel3
        - [ ] dummy
        - [ ] WViewProj
        - [ ] ViewProjection
        - [ ] viewProjIdentity
        - [ ] instance StrG.G ViewProjection
        - [ ] instance Default ViewProjection
    + [ ] SHADERS
        - [ ] glslVertexShader
        - [ ] glslFragmentShader
* [ ] Convert
* [ ] CreateTextureGroup
* [ ] Gpu.Vulkan.CairoImage
* [ ] KeyToKey
* [ ] KeyToKey.TH
* [ ] PangoLayoutExtent
* [ ] SampleImages
