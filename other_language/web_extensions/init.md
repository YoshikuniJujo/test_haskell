```
% mkdir try-foo
% cd try-foo
% mkdir src scripts
% npm init -y
% npm install --save-dev esbuild web-ext
% vim package.json
```

```
"type": "module"
```

```
% vim src/manifest.json
```

```
{
    "manifest_version": 3,
    "name": "Try Foo",
    "version": "0.1.0.0",
    "content_scripts": [
        {
            "matches": ["<all_urls>"],
            "js": ["content.js"]
        }
    ]
}
```

```
% vim src/content.js
```

```
document.documentElement.style.border = "5px solid red";
```

```
% vim scripts/build.js
```

```
import * as esbuild from "esbuild";
import { copyFile, mkdir, rm } from "node:fs/promises";

await rm("dist", { recursive: true, force: true });
await mkdir("dist");

await esbuild.build({
	entryPoints: ["src/content.js"],
	bundle: true,
	outfile: "dist/content.js"
});

await copyFile("src/manifest.json", "dist/manifest.json");
```

```
% vim scripts/run.js
```

```
import { spawn } from "node:child_process";

const p = spawn(
	"npx",
	[
		"web-ext", "run", "--source-dir", "dist",
		"--url", "https://yoshikunijujo.github.io/"
	],
	{ stdio: "inherit" }
);

p.on("exit", (code) => process.exit(code ?? 1));
```

```
% vim package.json
```

```
  "scripts": {
    "test": "echo \"Error: no test specified\" && exit 1",
    "build": "node scripts/build.js",
    "prerun": "npm run build",
    "run": "node scripts/run.js"
  },
}
```

```
% vim .gitignore
```

```
/node_modules/
/dist/
```
