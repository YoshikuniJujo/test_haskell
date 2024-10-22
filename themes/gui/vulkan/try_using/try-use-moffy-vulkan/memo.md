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
        - [ ] FramebufferResizedState
        - [ ] checkResizedState
        - [ ] WinEnvs
        - [ ] FramebufferResized
        - [ ] Swapchain
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
        - [ ] recreateFramebuffers'
        - [ ] SyncObjs
        - [ ] createSyncObjs
    + [ ] RECTANGLE BUFFER
    + [ ] GET GLFW EVENTS
    + [ ] CREATE AND COPY BUFFERS
    + [ ] GRAPHICS PIPELINE
    + [ ] MAINLOOP
    + [ ] RECREATE
    + [ ] DARW
    + [ ] DATA TYPES
    + [ ] SHADERS
* [ ] Convert
* [ ] CreateTextureGroup
* [ ] Gpu.Vulkan.CairoImage
* [ ] KeyToKey
* [ ] KeyToKey.TH
* [ ] PangoLayoutExtent
* [ ] SampleImages
