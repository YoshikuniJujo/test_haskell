app
---

* [x] try-cairo-image-as-texture.hs
* [ ] try-use-cairo.hs
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
* [ ] UseCairo
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
    + [ ] DRAW
        - [x] draw
        - [x] Draws
        - [x] winObjsToDraws
        - [ ] recordCmdBffr
        - [ ] bffrLstLn
        - [ ] updateViewProjBffr
    + [ ] RECTANGLES, VERTICES AND INDICES
    + [ ] SHADERS
    + [ ] export list
    + [ ] import list
* [ ] UseTextureGroup
* [ ] CreateTextureGroup
* [ ] SampleImages
* [ ] Vertex
* [ ] ThEnv
* [ ] Tools
* [ ] Convert
* [ ] Gpu.Vulkan.CairoImage
* [ ] PangoLayoutExtent
* [ ] Rectangles
* [ ] Rectangles2
