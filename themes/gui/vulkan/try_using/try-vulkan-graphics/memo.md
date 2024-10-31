### app

* [x] try-gamepad-button.hs
* [x] try-triangle-single.hs
* [x] try-rectangle-single.hs
* [x] try-depth-buffering-single.hs
* [x] try-cube-single.hs
* [x] try-multiple-windows.hs
* [x] try-compute.hs
    + [x] PARAMETERS
        - [x] MaxFramesInFlight
        - [x] TNum
        - [x] maxFramesInFlight
        - [x] class ToNum
        - [x] instance ToNum
        - [x] particleCount
    + [x] MAIN
        - [x] main
        - [x] FramebufferResized
        - [x] withWin
        - [x] createIst
        - [x] vldLayers
        - [x] dbgMsngrInfo
    + [x] BODY
        - [x] body
        - [x] pickPhd
        - [x] dvcExtensions
        - [x] findQFams
        - [x] QFamIndcs
        - [x] querySwpchSupport
        - [x] SwapchSupportDetails
        - [x] createLgDvc
        - [x] createCmdPl
        - [x] createDscPl
        - [x] createSyncObjs
        - [x] SyncObjs
        - [x] cmdBffrInfo
        - [x] createRndrPss
        - [x] createFrmbffrs
        - [x] class RecreateFrmbffrs
        - [x] instance RecreateFrmbffrs
        - [x] frmbffrInfo
    + [x] CREATE SWAP CHAIN
        - [x] createSwpch
        - [x] chooseSwpSfcFmt
        - [x] recreateSwpch
        - [x] querySwpchSupportFmt
        - [x] SwpchSuppotDetailsFmt
        - [x] swpExt
        - [x] choosePresentMode
        - [x] swpchInfo
        - [x] createImgVws
        - [x] recreateImgVws
        - [x] imgVwInfo
    + [x] BUFFERS
        - [x] createVtxBffrs
        - [x] createDeltaTimeBffr
        - [x] AtomDiffTime
        - [x] createBffrAtm
        - [x] createBffrLst
        - [x] createBffrLsts
        - [x] LstBffr
        - [x] LstBffr'
        - [x] createBffr
        - [x] findMmType
        - [x] bffrInfo
        - [x] writeBffrLsts
        - [x] singleTimeCmds
    + [x] COMPUTE DESCRIPTOR SET
        - [x] createCmpDscSts
        - [x] cmpDscStInfo
        - [x] updateCmpDscSt
        - [x] cmpWriteDscStUniform
        - [x] cmpWriteDscStStorageN
        - [x] ListVertex
    + [x] COMPUTE PIPELINE
        - [x] createCmpPpl
        - [x] shaderMdInfo
        - [x] createCmpPplLyt
        - [x] createCmpDscStLyt
        - [x] CmpDscStLytArg
    + [x] GRAPHICS PIPELINE
        - [x] createPplLyt
        - [x] createGrPpl
        - [x] recreateGrPpl
        - [x] grPplInfo
        - [x] shdrSt
        - [x] GlslVertexShaderArgs
        - [x] GlslFragmentShaderArgs
        - [x] vwpSt
        - [x] clrBlnd
    + [x] MAINLOOP
        - [x] mainloop
        - [x] cmpRun
        - [x] run
        - [x] draw
        - [x] catchAndSerialize
        - [x] recordCmdBffr
        - [x] bffrLstLn
    + [x] RECREATE
        - [x] catchAndRecreate
        - [x] recreateAll
    + [x] DATA TYPES
        - [x] WVertex
        - [x] Vertex
        - [x] instance GStorable.G Vertex
        - [x] Pos
        - [x] Color
        - [x] vertices
        - [x] randomVertex
    + [x] SHADERS
    + [x] import list

### src

* [x] Vertex
