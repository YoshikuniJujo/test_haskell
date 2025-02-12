### app

#### bicubic-interpolation.hs

* unorm -> srgb
* Vk.Cmd.blitImage -> Vk.Cmd.blitImage2
* Vk.Cmd.pipelineBarrier -> Vk.Cmd.pipelineBarrier2
* Vk.Q.submit -> Vk.Q.submit2

##### refactoring

* [ ] KEY EVENTS
	+ [x] kCllbck
	+ [x] data K
	+ [x] keyToK
	+ [x] data PR
	+ [x] keyStateToPR
	+ [ ] procKey
* [ ] SWAP CHAIN
	+ [ ] createSwpch
	+ [ ] querySwpchSupport
	+ [ ] data SwpchSupportDetails
	+ [ ] deriving instance Show (SwpchSupportDetails fmts)
	+ [ ] swpchExtent
	+ [ ] chooseSwpSfcFmt
	+ [ ] swpchInfo

### src
