export class Mutex {

	constructor() { this.queue = []; this.locked = false; }

	async acquire()
	{
		while (this.locked)
			await new Promise(rv => this.queue.push(rv));
		this.locked = true;
	}

	release()
	{
		if (this.queue.length > 0) {
			const next = this.queue.shift(); next(); }
		else { this.locked = false; }
	}

}
