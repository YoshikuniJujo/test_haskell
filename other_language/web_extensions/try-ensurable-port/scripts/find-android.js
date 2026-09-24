import { spawn } from "node:child_process";

const p = spawn(
	"npx",
	[	"web-ext",
		"run", "--source-dir", "dist", "-t", "firefox-android" ],
	{ stdio: "inherit" }
);

p.on("exit", code => process.exit(code ?? 1));
