import { OptionsState } from "../src/optionsState.js";

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

const storage = new StorageSessionMock();

const state = await OptionsState.create(storage);

console.log(state);

state.setAccountName("Ali");

console.log(state);
