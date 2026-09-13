import * as esbuild from "esbuild";
import { copyFile, mkdir, rm } from "node:fs/promises";

await rm("dist", { recursive: true, force: true });
await mkdir("dist");

await esbuild.build({
	entryPoints: ["src/content.js"],
	bundle: true,
	outfile: "dist/content.js"
});

await esbuild.build({
	entryPoints: ["src/background.js"],
	bundle: true,
	outfile: "dist/background.js"
});

await esbuild.build({
	entryPoints: ["src/options.js"],
	bundle: true,
	outfile: "dist/options.js"
});

await esbuild.build({
	entryPoints: ["src/input.js"],
	bundle: true,
	outfile: "dist/input.js"
});

await copyFile("src/manifest.json", "dist/manifest.json");
await copyFile("src/options.html", "dist/options.html");
await copyFile("src/options.css", "dist/options.css");
await copyFile("src/input.html", "dist/input.html");
await copyFile("src/input.css", "dist/input.css");
