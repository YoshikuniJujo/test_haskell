export class
EnsurablePort
{
	#name;
	#port;

	constructor(name)
	{
		this.#name = name;
		this.#port = null;

		browser.runtime.onConnect.addListener(port => {
			if (port.name === name) this.#setPort(port);
		});
	}

	ensure()
	{
		if (this.#port === null)
			this.#setPort(browser.runtime.connect({ name: this.#name }));
		return this.#port;
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
}
