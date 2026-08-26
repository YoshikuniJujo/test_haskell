browser.runtime.sendMessage({ method: "contentStarted" });

const pendingRequests = new Map();
const requestsByAnswer = new Map();

const newPendingRequests = new Map();

const tryPasswordTab = {

	queryInput(answer)
	{
		return new window.Promise(async (rslv, rjct) => {
			try {
			const rid = crypto.randomUUID();
			pendingRequests.set(rid, { resolve: rslv, reject: rjct });
			const rs = requestsByAnswer.get(answer) ?? [];
			rs.push(rid);
			requestsByAnswer.set(answer, rs);
			browser.runtime.sendMessage(
				{ method: "queryPass", answer: answer } );
			const pss = await new Promise((rs, rj) => {
				newPendingRequests.set(rid,
					{ resolve: rs, reject: rj });
			});

			console.log(pss);

			rslv(pss);
			}
			catch(e) {
				rjct(e);
			}
		});
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
				const nrq = newPendingRequests.get(rid);
				if (!nrq) {
					console.error(
						"invalid input request",
						{ request: rid, answer: m.answer } );
					throw new Error("Invalid input request"); }
				nrq.resolve(m.value);
//				rq.resolve(m.value);
				pendingRequests.delete(rid);
				newPendingRequests.delete(rid);
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
				const nrq = newPendingRequests.get(rid);
				if (!nrq) {
					console.error(
						"invalid input request",
						{ request: rid, answer: m.answer } );
					throw new Error("Invalid input request"); }
				pendingRequests.delete(rid);
				newPendingRequests.delete(rid);
				nrq.reject(
					new window.Error("Input tab was closed for answer: " +
						m.answer) ); }
//				rq.reject(
//					new window.Error("Input tab was closed for answer: " +
//						m.answer) ); }
			requestsByAnswer.delete(m.answer);
			break; } } });

window.wrappedJSObject.tryPasswordTab =
	cloneInto(tryPasswordTab, window, { cloneFunctions: true });
