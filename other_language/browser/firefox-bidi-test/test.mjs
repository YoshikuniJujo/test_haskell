import WebDriver from "webdriver";

const browser = await WebDriver.newSession({
	capabilities: {
		browserName: "firefox",
		webSocketUrl: true
	}
});

try {
	await browser.navigateTo(
		"file:///home/tatsuya/project/test_haskell/other_language/browser/firefox-bidi-test/test.html"
	);

	const button = await browser.findElement(
		"css selector",
		"#test"
	);

	await browser.elementClick(
		button["element-6066-11e4-a52e-4f735466cecf"]
	);

	console.log("button clicked");
}
finally {
	await browser.deleteSession();
}
