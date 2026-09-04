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

await copyFile("src/manifest.json", "dist/manifest.json");
