### app

#### bicubic-interpolation.hs

* unorm -> srgb
* Vk.Cmd.blitImage -> Vk.Cmd.blitImage2
* Vk.Cmd.pipelineBarrier -> Vk.Cmd.pipelineBarrier2
* Vk.Q.submit -> Vk.Q.submit2

##### refactoring

* [x] KEY EVENTS
	+ [x] kCllbck
	+ [x] data K
	+ [x] keyToK
	+ [x] data PR
	+ [x] keyStateToPR
	+ [x] procKey
* [x] SWAP CHAIN
	+ [x] createSwpch
	+ [x] querySwpchSupport
	+ [x] data SwpchSupportDetails
	+ [x] deriving instance Show (SwpchSupportDetails fmts)
	+ [x] swpchExtent
	+ [x] chooseSwpSfcFmt
	+ [x] swpchInfo

### src
