export class Mutex {

	#queue: ((value: void | PromiseLike<void>) => void)[];
	#locked: boolean;

	constructor() { this.#queue = []; this.#locked = false; }

	async acquire()
	{
		if (this.#locked) await new Promise(rv => this.#queue.push(rv));
		this.#locked = true;
	}

	release()
	{
		if (this.#queue.length > 0) {
			const next = this.#queue.shift()!; next(); }
		else { this.#locked = false; }
	}

}
