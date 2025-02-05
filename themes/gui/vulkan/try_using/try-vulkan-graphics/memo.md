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
* [ ] MAIN
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
	+ [ ] keyCallback
	+ [ ] draw
	+ [ ] update
	+ [ ] bar
* [ ] BUFFER AND IMAGE
* [ ] COMMANDS
* [ ] RESULT BUFFER
* [ ] SWAP CHAIN

### src
