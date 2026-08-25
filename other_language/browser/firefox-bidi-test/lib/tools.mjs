import path from "node:path";
import { fileURLToPath } from "node:url";
import { WebDriver } from "webdriver";

export async function
inputText(browser, context, text)
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

export async function
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

export async function
withNewContexts(browser, act)
{
	const before = await browser.browsingContextGetTree({});
	await act();
	const after = await browser.browsingContextGetTree({});
	const beforeIds = before.contexts.map(c => c.context);
	const newContexts =
		after.contexts.filter(c => !beforeIds.includes(c.context));
	return newContexts;
}
