import { spawn } from "node:child_process";

const p = spawn(
	"npx",
	[
		"web-ext", "run", "--source-dir", "dist",
		"--firefox-profile=./web-ext-profile",
		"--url", "https://yoshikunijujo.github.io/others/try-indexeddb-login",
		"--url", "about:debugging#/runtime/this-firefox",
		"--url", "about:addons"
	],
	{ stdio: "inherit" }
);

p.on("exit", (code) => process.exit(code?? 1));
