#!/bin/sh

web-ext run \
	--source-dir src -t firefox-android \
	--android-device=$npm_config_android_device \
	--firefox-apk=org.mozilla.fenix
