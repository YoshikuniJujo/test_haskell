const APP_ID = "556b2913-820c-46e7-9f37-3e0d6a67e680"

export class
EnsurablePort
{
	#name;
	#port = null;
	#disconnect = false;

	constructor(name)
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

	#listener = p =>
	{
		if (p.name === this.#name) this.#setPort(p);
	}

	#setPort(p)
	{
		this.#port = p;
		p.onDisconnect.addListener(() => {
			if (this.#port !== p) return;
			this.#port = null;
			if (this.#disconnect) {
				console.log("DISPOSE"); this.#dispose(); }
		});
		this.#port.onMessage.addListener(m => {
			console.log("ensurablePort.js:", m);
			switch(m.method) {
				case this.#name + ":disconnect":
					console.log("DISCONNECT");
					this.#disconnect = true;
					this.#port.postMessage({
						method: this.#name + ":disconnect-ack" });
					break;
			}
		});

	}
}

export class
EnsurablePortList
{
	#ports;

	constructor()
	{
		this.#ports = new Map();

		browser.runtime.onConnect.addListener(port => {
			const nm = port.name;
			this.#setPort(nm, port);
		});
	}

	ensure(name, tid)
	{
		let port = this.#ports.get(name);
		if (port) return port;
		port = browser.tabs.connect(tid, { name: name });
		this.#setPort (name, port);
		return port;
	}

	#setPort(name, port)
	{
		this.#ports.set(name, port);
		port.onDisconnect.addListener(() => {
			if (this.#ports.get(name) === port) this.#ports.delete(name);
		});
	}
}
