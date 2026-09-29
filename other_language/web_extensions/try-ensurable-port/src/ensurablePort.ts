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
	#listeners: Array<(message: object) => void> = [];

	constructor(name: string)
	{
		this.#name = APP_ID + ":" + name;
		this.#port = null;

		browser.runtime.onConnect.addListener(this.#listener);
	}

	ensure(): browser.runtime.Port
	{
		if (this.#port === null) {
			const port = browser.runtime.connect({ name: this.#name });
			this.#setPort(port);
			this.#listeners.forEach(l => { port.onMessage.addListener(l); });
		}
		if (this.#port === null)
			throw new Error("failed to ensure port");
		return this.#port;
	}

	post(message: object)
	{
		this.ensure().postMessage(message);
	}

	addListener(listener: (message: object) => void)
	{
		const p = this.ensure();
		this.#listeners.push(listener);
		p.onMessage.addListener(listener);
	}

	disconnect()
	{
		this.ensure().postMessage({ method: `${this.#name}:disconnect` });
		this.#disconnect = true;
		this.#dispose();
	}

	#dispose()
	{
		browser.runtime.onConnect.removeListener(this.#listener);
	}

	#listener = (p: browser.runtime.Port) =>
	{
		console.log("EnsurablePort:", p.name, this.#name);
		if (p.name === this.#name) {
			this.#setPort(p);
			this.#listeners.forEach(l => { p.onMessage.addListener(l); });
		}
	}

	#setPort(p: browser.runtime.Port)
	{
		console.log("EnsurablePort: #setPort:", p);
		this.#port = p;
		p.onDisconnect.addListener(() => {
			if (this.#port !== p) return;
			this.#port = null;
			if (this.#disconnect) {
				console.log("DISPOSE"); this.#dispose(); }
		});
		p.onMessage.addListener(m => {
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
				case this.#name + ":disconnect-ack":
					console.log("EnsurablePort: DISCONNECT ACK");
					this.#port!.disconnect();
					break;
			}
			}
		});

	}
}

type DisconnectMessage<N extends string> = {
	method: `${typeof APP_ID}:${N}:disconnect`;
};

function
isDisconnectMessage<N extends string>(m: object, nm: N): m is DisconnectMessage<N> {
	return "method" in m
		&& m.method === `${APP_ID}:${nm}:disconnect`;
}

export class
EnsurablePortList
{
	#ports = new Map<string, browser.runtime.Port>();
	#disconnects = new Map<string, boolean>();

	constructor()
	{
		console.log("EnsurablePortList: constructor()");

		browser.runtime.onConnect.addListener(this.#listener);
	}

	post(name: string, message: object, tid?: number)
	{
		console.log("EnsurablePortList: post", name, message, tid);
		const port = this.#ports.get(name);

		if (port) {
			port.postMessage(message);
			return;
		}

		if (tid !== undefined) this.#postWithTab(name, message, tid);
		else this.#postWithoutTab(name, message);
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
		console.log("EnsurablePortList: #setPort", name, port);
		this.#ports.set(name, port);
		port.onDisconnect.addListener(() => {
			console.log("EnsurablePortList: disconnect", name, port);
			if (this.#ports.get(name) === port) this.#ports.delete(name);
		});
		port.onMessage.addListener(m => {
			console.log("EnsurablePortList: onMessage:", m);
			if (!isDisconnectMessage(m, name)) throw new Error("m is not DisconnectMessage");
			switch (m.method) {
				case `${APP_ID}:${name}:disconnect`:
					console.log("EnsurablePortList: DISCONNECT");
					this.#disconnects.set(name, true);
					if ([...this.#ports.keys()].every(
						nm => this.#disconnects.get(nm) === true )) {
						browser.runtime.onConnect.removeListener(this.#listener);
						console.log("EnsurablePortList: after remove runtime listener");
						}
					port.postMessage({
						method: `${APP_ID}:${name}:disconnect-ack` });
					console.log("EnsurablePortList: after remove listener");
					break;
			}
		});
	}

	#listener = async (p: browser.runtime.Port) =>
	{
		const name = p.name
		console.log("EnsurablePortList: #listener:", name);
		const nm = name.split(":").pop();
		if (!nm) throw(new Error("bad"));
		const key = name;
		this.#setPort(nm, p);
		const result = await browser.storage.session.get(key);
		const queue = result[key] ?? [];
		console.log("EnsurablePortList:#listener:", queue);
		queue.forEach((m: object) => { p.postMessage(m); });
		await browser.storage.session.set( { [key]: [] });
	}

	#postWithTab(name: string, message: object, tid: number)
	{
		const p = browser.tabs.connect(tid, { name: `${APP_ID}:${name}` });
		this.#setPort(name, p);
		p.postMessage(message);
	}

	#queue: Promise<void> = Promise.resolve();

	async #postWithoutTab(name: string, message: object)
	{
		const key = `${APP_ID}:${name}`;
		this.#queue = this.#queue.then(async () => {
		const result = await browser.storage.session.get(key);
		const queue = result[key] ?? [];
		queue.push(message);
		console.log("*** QUEUE:", queue);
		await browser.storage.session.set({
			[key]: queue
		});
		});
	}
}
