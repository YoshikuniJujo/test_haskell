import { spawn } from "node:child_process";

const p = spawn(
	"npx",
	[	"web-ext", "run",
		"--source-dir", "dist", "-t", "firefox-android",
		`--android-device=${process.env.npm_config_android_device}`,
		"--firefox-apk=org.mozilla.fenix" ],
	{ stdio: "inherit" }
);

p.on("exit", code => process.exit(code ?? 1));
