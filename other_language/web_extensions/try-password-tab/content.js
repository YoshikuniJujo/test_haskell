document.body.style.border = "3px solid green";

const test = {

	openInputTab() {

		browser.runtime.sendMessage({
			method: "openInputTab"
		});

	}

};

window.wrappedJSObject.test =
	cloneInto(test, window, {
		cloneFunctions: true
	});
