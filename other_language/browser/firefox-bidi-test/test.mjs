import WebDriver from "webdriver";

const browser = await WebDriver.newSession({
	capabilities: {
		browserName: "firefox",
		webSocketUrl: true
	}
});

try {
	const result = await browser.webExtensionInstall({
		extensionData: {
			type: "path",
			path: "/home/tatsuya/project/test_haskell/other_language/web_extensions/try-password-tab/src"
		}
	});

	console.log("extension: ", result);

	await browser.navigateTo(
		"https://yoshikunijujo.github.io/others/try-password-tab/"
	);

	const before = await browser.browsingContextGetTree({});

	const mainContext =
		before.contexts.find(
			c => c.url ===
				"https://yoshikunijujo.github.io/others/try-password-tab/"
		);

	console.log("SOURCE CONTEXT");
	console.log(mainContext);

	await click(browser, mainContext, "#open-input1");

	console.log("button clicked");

	const after = await browser.browsingContextGetTree({});

	console.log("before", before);
	console.log("after", after);
	const beforeIds = before.contexts.map(c => c.context);
	console.log("beforeIds", beforeIds);
	const newContexts =
		after.contexts.filter(
			c => !beforeIds.includes(c.context)
		);
	console.log("after-before", newContexts);
	console.log("after-before[0]", newContexts[0]);
	const inputContext = newContexts[0];

	await new Promise(resolve => setTimeout(resolve, 1000));

	const result2 = await browser.browsingContextLocateNodes({
		context: inputContext.context,
		locator: {
			type: "css",
			value: "#send"
		}
	});

	const send = result2.nodes[0];

	console.log("HERE");
	console.log(result2);
	console.log(send);

	const result3 = await browser.browsingContextLocateNodes({
		context: inputContext.context,
		locator: {
			type: "css",
			value: "#input"
		}
	});

	const input = result3.nodes[0];

	console.log("HERE2");
	console.log(result3);
	console.log(input);

	console.log(textToKeyActions("p"));

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

	await browser.inputPerformActions({
		context: inputContext.context,
		actions: [{
			type: "pointer",
			id: "mouse",
			parameters: {
				pointerType: "mouse"
			},
			actions: [
				{
					type: "pointerMove",
					x: 0,
					y: 0,
					origin: {
						type: "element",
						element: send
					}
				},
				{
					type: "pointerDown",
					button: 0
				},
				{
					type: "pointerUp",
					button: 0
				}
			]
		}]
	});

	await browser.inputReleaseActions({
		context: inputContext.context
	});

	const result4 = await browser.scriptCallFunction({
		functionDeclaration:
			'() => document.querySelector("#result1").textContent',
		awaitPromise: false,
		target: {
			type: "context",
			context: mainContext.context
		}
	});

	console.log("result: ", result4);

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
		actions: [{
			type: "pointer",
			id: "mouse",
			parameters: {
				pointerType: "mouse"
			},
			actions: [
				{
					type: "pointerMove",
					x: 0,
					y: 0,
					origin: {
						type: "element",
						element
					}
				},
				{
					type: "pointerDown",
					button: 0
				},
				{
					type: "pointerUp",
					button: 0
				}
			]
		}]
	});

	await browser.inputReleaseActions({
		context: context.context
	});
}
