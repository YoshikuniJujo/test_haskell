browser.runtime.sendMessage({ method: "contentStarted" });

const pendingRequests = new Map();
const requestsByAnswer = new Map();

const tryPasswordTab = {

	queryInput(answer)
	{
		return new window.Promise((rslv, rj) => {
			const rid = crypto.randomUUID();
			pendingRequests.set(rid, { resolve: rslv, reject: rj });
			const rs = requestsByAnswer.get(answer) ?? [];
			rs.push(rid);
			requestsByAnswer.set(answer, rs);
			browser.runtime.sendMessage(
				{ method: "queryPass", answer: answer } ); });
	}

};

browser.runtime.onMessage.addListener((m) => {
	switch (m.method) {
		case "pushPass": {
			console.log(typeof m.answer, m.answer);
			console.log(
				[...requestsByAnswer.keys()].map(k => [typeof k, k]) );
			console.log(requestsByAnswer);
			const rids = requestsByAnswer.get(m.answer);
			console.log(rids);
			for (const rid of rids) {
				const rq = pendingRequests.get(rid);
				if (!rq) {
					console.error(
						"invalid input request",
						{ request: rid, answer: m.answer } );
					throw new Error("Invalid input request"); }
				rq.resolve(m.value);
				pendingRequests.delete(rid);
			}
			requestsByAnswer.delete(m.answer);
			break; }
		case "passError": {
			console.log(typeof m.answer, m.answer);
			console.log(
				[...requestsByAnswer.keys()].map(k => [typeof k, k]) );
			console.log(requestsByAnswer);
			const rids = requestsByAnswer.get(m.answer);
			console.log(rids);
			for (const rid of rids) {
				const rq = pendingRequests.get(rid);
				if (!rq) {
					console.error(
						"invalid input request",
						{ request: rid, answer: m.answer } );
					throw new Error("Invalid input request"); }
				pendingRequests.delete(rid);
				rq.reject(
					new window.Error("Input tab was closed for answer: " +
						m.answer) ); }
			requestsByAnswer.delete(m.answer);
			break; } } });

window.wrappedJSObject.tryPasswordTab =
	cloneInto(tryPasswordTab, window, { cloneFunctions: true });
