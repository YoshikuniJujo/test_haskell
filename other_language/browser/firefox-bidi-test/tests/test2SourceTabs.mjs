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
	const mainContext1 =
		(await browser.browsingContextGetTree({})).contexts[0];
	const mainContext2 =
		await browser.browsingContextCreate({ type: "tab" });

	await browser.browsingContextNavigate({
		context: mainContext1.context, url: testPagePath });
	await browser.browsingContextNavigate({
		context: mainContext2.context, url: testPagePath });
	await new Promise(resolve => setTimeout(resolve, 1000));

	const inputContext = (await withNewContexts(browser, async () => {
		await click(browser, mainContext1, "#open-input1");
		await click(browser, mainContext1, "#open-input2");
		await click(browser, mainContext1, "#open-input3");
		await click(browser, mainContext2, "#open-input1");
		await click(browser, mainContext2, "#open-input3");
	}))[2];
	await new Promise(resolve => setTimeout(resolve, 1000));

	await browser.browsingContextClose({
		context: mainContext1.context });

	await new Promise(resolve => setTimeout(resolve, 1000));

	await inputText(browser, inputContext, "password");
	await click(browser, inputContext, "#send");
	const rslt = await browser.scriptCallFunction({
		functionDeclaration:
			'() => document.querySelector("#result1").textContent',
		awaitPromise: false,
		target: { type: "context", context: mainContext2.context } });
	const actual = rslt.result.value;
	if (actual !== "結果: password")
		throw new Error(`err: expected="password", actual="${actual}"`);

	await new Promise(resolve => setTimeout(resolve, 2000));
}
finally
{
	await browser.deleteSession();
}
