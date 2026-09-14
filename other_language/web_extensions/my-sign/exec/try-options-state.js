import { OptionsState } from "../src/optionsState.js";

console.log(new OptionsState());

class StorageSessionMock {
	constructor() {
		this.data = new Map();
	}

	async get(key) {
		return {
			[key]: this.data.get(key)
		};
	}

	async set(values) {
		for (const [key, value] of Object.entries(values))
			this.data.set(key, value);
	}
}
