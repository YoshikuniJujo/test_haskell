import path from "node:path";
import { fileURLToPath } from "node:url";
import { WebDriver } from "webdriver";

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
	const inputContext = (await withNewContexts(() =>
		click(browser, mainContext, "#open-input1")))[0];
	await new Promise(resolve => setTimeout(resolve, 1000));

	await inputText(inputContext, "password");
	await click(browser, inputContext, "#send");

	const result = await browser.scriptCallFunction({
		functionDeclaration:
			'() => document.querySelector("#result1").textContent',
		awaitPromise: false,
		target: { type: "context", context: mainContext.context } });
	const rslt1 = result.result.value;
	if (result.result.value !== "結果: password")
		throw new Error(`err: expected="password", actual="${rslt1}"`);
	await new Promise(resolve => setTimeout(resolve, 1000));
}
finally
{
	await browser.deleteSession();
}

async function
inputText(context, text)
{
	await browser.inputPerformActions({
		context: context.context, actions: [textToKeyAction(text)] });
}

function
textToKeyAction(text)
{
	return {
		type: "key", id: "keyboard",
		actions: textToKeyActions(text) };
}

function
textToKeyActions(text)
{
	return [...text].flatMap(c => [
		{ type: "keyDown", value: c }, { type: "keyUp", value: c } ]);
}

async function
click(browser, context, selector)
{
	await browser.inputPerformActions({
		context: context.context,
		actions: [await clickAction(browser, context, selector)] });
}

async function
clickAction(browser, context, selector)
{
	return {
		type: "pointer",
		id: "mouse",
		parameters: { pointerType: "mouse" },
		actions: await clickAction_(browser, context, selector) }
}

async function
clickAction_(browser, context, selector)
{
	const result = await browser.browsingContextLocateNodes({
		context: context.context,
		locator: {
			type: "css",
			value: selector
		}
	});

	if (result.nodes.length !== 1)
		throw new Error(`Element not found: ${selector}`);

	const element = result.nodes[0];
	return [
		{	type: "pointerMove", x: 0, y: 0,
			origin: { type: "element", element } },
		{ type: "pointerDown", button: 0 },
		{ type: "pointerUp", button: 0 } ]
}

async function
withNewContexts(act)
{
	const before = await browser.browsingContextGetTree({});
	await act();
	const after = await browser.browsingContextGetTree({});
	const beforeIds = before.contexts.map(c => c.context);
	const newContexts =
		after.contexts.filter(c => !beforeIds.includes(c.context));
	return newContexts;
}
