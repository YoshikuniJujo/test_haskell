import path from "node:path";
import { fileURLToPath } from "node:url";
import { WebDriver } from "webdriver";

import { click, inputText, withNewContexts } from "../lib/tools.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const extensionPath = path.resolve(
	__dirname, "../../../web_extensions/try-password-tab/src" );
const testPagePath = "https://yoshikunijujo.github.io/others/try-password-tab/"
const browser = await WebDriver.newSession({
	capabilities: { browserName: "firefox", webSocketUrl: true } });

try {
	await browser.webExtensionInstall({
		extensionData: { type: "path", path: extensionPath } });
	const mainContext =
		(await browser.browsingContextGetTree({})).contexts[0];
	await browser.browsingContextNavigate({
		context: mainContext.context, url: testPagePath });
	const inputContext = (await withNewContexts(browser, async () => {
		await new Promise(resolve => setTimeout(resolve, 100));
		await click(browser, mainContext, "#open-input1")
		await click(browser, mainContext, "#open-input2")
		await click(browser, mainContext, "#open-input3")
	} ))[1];

	await inputText(browser, inputContext, "password456");
	await click(browser, inputContext, "#send");

	await new Promise(resolve => setTimeout(resolve, 1000));

	const rslt = await browser.scriptCallFunction({
		functionDeclaration:
			'() => document.querySelector("#result1").textContent',
		awaitPromise: false,
		target: { type: "context", context: mainContext.context } });
	const actual = rslt.result.value;
	if (actual !== "結果: password456")
		throw new Error(`err: expected="password456", actual="${actual}"`);
	await new Promise(resolve => setTimeout(resolve, 1000));

	const emptyContext = await browser.browsingContextCreate({
		type: "tab"
	});

	await browser.browsingContextClose({
		context: mainContext.context
	});
	await new Promise(resolve => setTimeout(resolve, 1000));
}
finally
{
	await browser.deleteSession();
}
