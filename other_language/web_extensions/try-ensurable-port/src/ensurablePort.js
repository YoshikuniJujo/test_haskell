export class
EnsurablePort
{
	#name;
	#port;

	constructor(name)
	{
		this.#name = name;
		this.#port = null;

		browser.runtime.onConnect.addListener(this.#listener);
	}

	ensure()
	{
		if (this.#port === null)
			this.#setPort(browser.runtime.connect({ name: this.#name }));
		return this.#port;
	}

	dispose()
	{
		if (this.#port) {
			this.#port.disconnect();
			this.#port = null; }
		browser.runtime.onConnect.removeListener(this.#listener);
	}

	#listener(p)
	{
		if (p.name === name) this.#setPort(p);
	}

	#setPort(p)
	{
		this.#port = p;
		p.onDisconnect.addListener(() => {
			if (this.#port === p) this.#port = null; });

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
