browser.runtime.sendMessage({ method: "contentStarted" });

const requestsByAnswer = new Map();

const pendingPassword = new Map();
const newPendingRequests = new Map();

const tryPasswordTab = {

	queryInput(answer)
	{
		return new window.Promise(async (rslv, rjct) => {
			try {
				const rid = crypto.randomUUID();
				const rs = requestsByAnswer.get(answer) ?? [];
				rs.push(rid);
				requestsByAnswer.set(answer, rs);
				browser.runtime.sendMessage(
					{ method: "queryPass", answer: answer } );
				pendingPassword.set(rid, rslv);
				await new Promise((rs, rj) => {
					newPendingRequests.set(rid,
						{ resolve: rs, reject: rj });
				});

				browser.runtime.sendMessage(
					{ method: "getSomething", answer: answer, request: rid } );

			}

//				rslv(pss); }
			catch(e) {
				rjct(e);
			}
		});
	}

};

browser.runtime.onMessage.addListener((m) => {
	switch (m.method) {
		case "something": {
			console.log(m.request);
			console.log(m.value);
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
			console.log(typeof m.answer, m.answer);
			console.log(
				[...requestsByAnswer.keys()].map(k => [typeof k, k]) );
			console.log(requestsByAnswer);
			const rids = requestsByAnswer.get(m.answer);
			console.log(rids);
			for (const rid of rids) {
				const nrq = newPendingRequests.get(rid);
				if (!nrq) {
					console.error(
						"invalid input request",
						{ request: rid, answer: m.answer } );
					throw new Error("Invalid input request"); }
				nrq.resolve();
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
				const nrq = newPendingRequests.get(rid);
				if (!nrq) {
					console.error(
						"invalid input request",
						{ request: rid, answer: m.answer } );
					throw new Error("Invalid input request"); }
				newPendingRequests.delete(rid);
				nrq.reject(
					new window.Error("Input tab was closed for answer: " +
						m.answer) ); }
			requestsByAnswer.delete(m.answer);
			break; } } });

window.wrappedJSObject.tryPasswordTab =
	cloneInto(tryPasswordTab, window, { cloneFunctions: true });
