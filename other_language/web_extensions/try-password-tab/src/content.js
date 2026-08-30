browser.runtime.sendMessage({ method: "contentStarted" });

const requestsWaitingForAnswer = new Map();
const pendingPassword = new Map();

const tryPasswordTab = {

	getSomething(aid, parameter)
	{
		return new window.Promise(async (rslv, rjct) => {
			try {
				browser.runtime.sendMessage( {
					method: "queryPass", answer: aid } );
				await new Promise((rs, rj) => {
					addToArrayMap(requestsWaitingForAnswer, aid, { resolve: rs, reject: rj });
				});

				const rid = crypto.randomUUID();
				pendingPassword.set(rid, rslv);
				browser.runtime.sendMessage(
					{ method: "getSomething", answer: aid, request: rid, parameter } );

			}

			catch(e) {
				rjct(e);
			}
		});
	}

};

browser.runtime.onMessage.addListener((m) => {
	switch (m.method) {
		case "something": {
			const rslv = pendingPassword.get(m.request);
			pendingPassword.delete(m.request);
			if (!rslv) {
				console.error(
					"invalid input request",
					{ request: m.request } );
				throw new Error("Invalid input request"); }
			rslv(m.value);
			break;
		}
		case "passwordReady": {
			const rids = requestsWaitingForAnswer.get(m.answer);
			for (const rid of rids) {
				/*
				const nrq = newPendingRequests.get(rid);
				if (!nrq) {
					console.error(
						"invalid input request",
						{ request: rid, answer: m.answer } );
					throw new Error("Invalid input request"); }
					*/
				rid.resolve();
			}
			requestsWaitingForAnswer.delete(m.answer);
			break; }
		case "passError": {
			console.log(typeof m.answer, m.answer);
			console.log(
				[...requestsWaitingForAnswer.keys()].map(k => [typeof k, k]) );
			console.log(requestsWaitingForAnswer);
			const rids = requestsWaitingForAnswer.get(m.answer);
			console.log(rids);
			for (const rid of rids) {
				/*
				const nrq = newPendingRequests.get(rid);
				if (!nrq) {
					console.error(
						"invalid input request",
						{ request: rid, answer: m.answer } );
					throw new Error("Invalid input request"); }
					*/
				rid.reject(
					new window.Error("Input tab was closed for answer: " +
						m.answer) ); }
			requestsWaitingForAnswer.delete(m.answer);
			break; } } });

function
addToArrayMap(map, key, value)
{
	console.log("addToArrayMap");
	const values = map.get(key);
	if (values) values.push(value); else map.set(key, [value]);
	console.log(map);
}

window.wrappedJSObject.tryPasswordTab =
	cloneInto(tryPasswordTab, window, { cloneFunctions: true });
