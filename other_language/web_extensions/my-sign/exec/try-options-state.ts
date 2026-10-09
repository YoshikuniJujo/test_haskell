import { OptionsState } from "../src/optionsState.js";
import { optionsTabTag } from "../src/types.js";

class StorageSessionMock<T> {

	#data;

	constructor() {
		this.#data = new Map();
	}

	async get(key: string) {
		return {
			[key]: this.#data.get(key)
		};
	}

	async set(values: Record<string,T>) {
		for (const [key, value] of Object.entries(values))
			this.#data.set(key, value);
	}
}

const storage = new StorageSessionMock<object>();

const state = new OptionsState(optionsTabTag("browser"), storage);

console.log(state);

await state.write("set", "accountName", "Ali");

await state.write("set", "accountName", "Alice");

const r = state.write("generateAccount", "password", "hogepiyo");

console.log(r);
