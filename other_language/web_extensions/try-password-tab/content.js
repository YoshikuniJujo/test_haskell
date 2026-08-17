document.body.style.border = "3px solid green";

const test = {

	openInputTab() {
		return new window.Promise(async (resolve) => {

			const r = await browser.runtime.sendMessage({
				method: "openInputTab"
			});

			console.log("content result: ", r);

			resolve(r);
		});

	}

};

window.wrappedJSObject.test =
	cloneInto(test, window, {
		cloneFunctions: true
	});
