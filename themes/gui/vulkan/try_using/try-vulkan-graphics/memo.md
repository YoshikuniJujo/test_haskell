### app

* [ ] bicubic-interpolation.hs
	+ [ ] runCmds
		- [ ] separate command buffer allocation
			* [ ] reset command buffer
	+ [ ] use Vk.Q.submit2
	+ [ ] use Vk.Cmd.blitImage2
	+ [ ] use Vk.Cmd.pipelineBarrier2

#### bicubic-interpolation.hs

* unorm -> srgb
* Vk.Cmd.blitImage -> Vk.Cmd.blitImage2
* Vk.Cmd.pipelineBarrier -> Vk.Cmd.pipelineBarrier2
* Vk.Q.submit -> Vk.Q.submit2

##### refactoring

* [x] DATA TYPE IMAGE RGBA8
* [x] MAIN
	+ [x] main
	+ [x] getFilter
	+ [x] newtype Filter
	+ [x] realMain
	+ [x] createIst
	+ [x] vldLayers
	+ [x] pickPhd
	+ [x] createLgDvc
	+ [x] createCmdPl
	+ [x] type ShaderFormat
	+ [x] body
	+ [x] resultBffr
	+ [x] imgVwInfo
	+ [x] type PshCnsts
	+ [x] strgImgBinding
	+ [x] withWindow
	+ [x] waitFramebufferSize
	+ [x] kCllbck
	+ [x] draw
		- [x] type declaration
		- [x] function definition
	+ [x] update
	+ [x] bar
* [x] BUFFER AND IMAGE
	+ [x] createBffrImg
	+ [x] createBffr
	+ [x] findMmType
	+ [x] bffrInfo
	+ [x] prepareImg
* [x] COMMAND BUFFER
	+ [x] allocateCmdBffr
	+ [x] runCmds
	+ [x] submitINfo
	+ [x] smphInfo
* [x] COMMANDS
	+ [x] copyBffrToImg
	+ [x] colorLayer0
	+ [x] bffrImgExtent
	+ [x] transitionImgLyt
	+ [x] copyImgToImg
	+ [x] copyImgToBffr
* [ ] PIPELINE AND DESCRIPTOR SET
	+ [ ] createCmpPpl
	+ [ ] createPplLyt
	+ [ ] createDscStLyt
	+ [ ] createDscPl
	+ [ ] createDscSt
	+ [ ] createDscStSrc
	+ [ ] type SrcImg
	+ [ ] type DstImg
	+ [ ] dscWrite
	+ [ ] compileShader
* [ ] SWAP CHAIN
* [ ] TOOLS

### src
