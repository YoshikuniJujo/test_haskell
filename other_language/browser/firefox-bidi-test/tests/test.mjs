import path from "node:path";
import { fileURLToPath } from "node:url";
import WebDriver from "webdriver";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const extensionPath = path.resolve(
	__dirname, "../../../web_extensions/try-password-tab/src" );
const testPagePath = "https://yoshikunijujo.github.io/others/try-password-tab/"

const browser = await WebDriver.newSession({
	capabilities: { browserName: "firefox", webSocketUrl: true } });

try {
	await browser.webExtensionInstall({
		extensionData: { type: "path", path: extensionPath } });
	await browser.navigateTo(testPagePath);

	const before = await browser.browsingContextGetTree({});

	const mainContext =
		before.contexts.find(
			c => c.url ===
				"https://yoshikunijujo.github.io/others/try-password-tab/"
		);

	await click(browser, mainContext, "#open-input1");

	const after = await browser.browsingContextGetTree({});
	const beforeIds = before.contexts.map(c => c.context);
	const newContexts =
		after.contexts.filter(
			c => !beforeIds.includes(c.context)
		);
	const inputContext = newContexts[0];

	await new Promise(resolve => setTimeout(resolve, 1000));

	const result3 = await browser.browsingContextLocateNodes({
		context: inputContext.context,
		locator: {
			type: "css",
			value: "#input"
		}
	});

	const input = result3.nodes[0];

	await browser.inputPerformActions({
		context: inputContext.context,
		actions: [
			{
				type: "key",
				id: "keyboard",
				actions: textToKeyActions("password")
			}
		]
	});

	await click(browser, inputContext, "#send");

	const result4 = await browser.scriptCallFunction({
		functionDeclaration:
			'() => document.querySelector("#result1").textContent',
		awaitPromise: false,
		target: {
			type: "context",
			context: mainContext.context
		}
	});

	const actual = result4.result.value;

	if (actual !== "結果: password")
		throw new Error(
			`結果が違います: expected="password", actual="${actual}"`
		);

	await new Promise(resolve => setTimeout(resolve, 1000));
}
finally {
	await browser.deleteSession();
}

function
textToKeyActions(text)
{
	return [...text].flatMap(c => [
		{ type: "keyDown", value: c },
		{ type: "keyUp", value: c }
	]);
}

async function
click(browser, context, selector)
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

	await browser.inputPerformActions({
		context: context.context,
		actions:

		[{
			type: "pointer",
			id: "mouse",
			parameters: {
				pointerType: "mouse"
			},
			actions: clickAction(element)
		}]
	});
}

function
clickAction(element)
{
	return [
		{	type: "pointerMove", x: 0, y: 0,
			origin: { type: "element", element } },
		{ type: "pointerDown", button: 0 },
		{ type: "pointerUp", button: 0 } ]
}
