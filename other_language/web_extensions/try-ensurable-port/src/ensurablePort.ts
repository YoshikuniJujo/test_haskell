const APP_ID = "556b2913-820c-46e7-9f37-3e0d6a67e680"

type MessageWithMethod = {
	method: string;
};

function hasMethod(m: object): m is MessageWithMethod {
	return "method" in m && typeof m.method === "string";
}

export class
EnsurablePort
{
	#name: string;
	#port: browser.runtime.Port | null = null;
	#disconnect = false;

	constructor(name: string)
	{
		this.#name = APP_ID + ":" + name;
		this.#port = null;

		browser.runtime.onConnect.addListener(this.#listener);
	}

	ensure()
	{
		if (this.#port === null)
			this.#setPort(browser.runtime.connect({ name: this.#name }));
		return this.#port;
	}

	#dispose()
	{
		browser.runtime.onConnect.removeListener(this.#listener);
	}

	#listener = (p: browser.runtime.Port) =>
	{
		if (p.name === this.#name) this.#setPort(p);
	}

	#setPort(p: browser.runtime.Port)
	{
		this.#port = p;
		p.onDisconnect.addListener(() => {
			if (this.#port !== p) return;
			this.#port = null;
			if (this.#disconnect) {
				console.log("DISPOSE"); this.#dispose(); }
		});
		this.#port.onMessage.addListener(m => {
			if (hasMethod(m)) {
			console.log("ensurablePort.js:", m);
			console.log("ensurablePort.js: this.#name =", this.#name);
			console.log("ensurablePort.js: m =", m);
			console.log("ensurablePort.js: m.method =", m.method);
			switch(m.method) {
				case this.#name + ":disconnect":
					console.log("DISCONNECT");
					this.#disconnect = true;
					this.#port!.postMessage({
						method: this.#name + ":disconnect-ack" });
					break;
			}
			}
		});

	}
}

export class
EnsurablePortList
{
	#ports = new Map<string, browser.runtime.Port>();

	constructor()
	{
		console.log("EnsurablePortList: constructor()");

		browser.runtime.onConnect.addListener(this.#listener);
	}

	ensure(name: string, tid: number)
	{
		let port = this.#ports.get(name);
		if (port) return port;
		port = browser.tabs.connect(tid, { name: `${APP_ID}:${name}` });
		this.#setPort (name, port);
		return port;
	}

	#setPort(name: string, port: browser.runtime.Port)
	{
		this.#ports.set(name, port);
		port.onDisconnect.addListener(() => {
			if (this.#ports.get(name) === port) this.#ports.delete(name);
		});
		port.onMessage.addListener(m => {
			console.log("EnsurablePortList: onMessage:", m);
		});
	}

	#listener = (p: browser.runtime.Port) =>
	{
		const nm = p.name.split(":").pop();
		if (!nm) throw(new Error("bad"));
		this.#setPort(nm, p);
	}
}
