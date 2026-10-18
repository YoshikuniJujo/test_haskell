import * as esbuild from "esbuild";
import { copyFile, mkdir, rm } from "node:fs/promises";

const dev = process.argv.includes("--dev");

const buildOptions = {
	bundle: true,
	define: {
		__DEV__: dev ? "true" : "false"
	}
};

await rm("dist", { recursive: true, force: true });
await mkdir("dist");

await esbuild.build({
	...buildOptions,
	entryPoints: ["src/content.ts"],
	bundle: true,
	outfile: "dist/content.js"
});

await esbuild.build({
	...buildOptions,
	entryPoints: ["src/background.ts"],
	bundle: true,
	outfile: "dist/background.js"
});

await esbuild.build({
	...buildOptions,
	entryPoints: ["src/options.ts"],
	bundle: true,
	outfile: "dist/options.js"
});

await esbuild.build({
	...buildOptions,
	entryPoints: ["src/input.ts"],
	bundle: true,
	outfile: "dist/input.js"
});

await copyFile("src/manifest.json", "dist/manifest.json");
await copyFile("src/options.html", "dist/options.html");
await copyFile("src/options.css", "dist/options.css");
await copyFile("src/input.html", "dist/input.html");
await copyFile("src/input.css", "dist/input.css");
