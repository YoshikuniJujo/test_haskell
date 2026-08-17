document.body.style.border = "3px solid green";

const pendingRequests = new Map();

const test = {

	openInputTab() {
		return new window.Promise(async (resolve) => {

			const r = await browser.runtime.sendMessage({
				method: "openInputTab"
			});

			console.log("content result: ", r);

			resolve(r);
		});

	},

	openInputTab2() {
		return new window.Promise(async (resolve, reject) => {

			console.log("here");

			const requestId = crypto.randomUUID();

			pendingRequests.set(requestId, { resolve, reject });

			resolve("foobar");
		});
	}

};

window.wrappedJSObject.test =
	cloneInto(test, window, {
		cloneFunctions: true
	});
