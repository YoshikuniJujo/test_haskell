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

	await browser.sessionSubscribe({
		events: ["browsingContext.contextCreated"]
	});

	const button = await browser.findElement(
		"css selector",
		"#open-input1"
	);

	await browser.elementClick(
		button["element-6066-11e4-a52e-4f735466cecf"]
	);

	console.log("button clicked");

//	const event = await created;

//	console.log("created: ", event);

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
}
finally {
	await browser.deleteSession();
}
