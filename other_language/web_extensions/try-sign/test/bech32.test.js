import { test } from "node:test";
import assert from "node:assert/strict";
import { decode, encode } from "../src/codec/bech32.js";


test("npub round trip", () => {
	const npub = "npub1" +
		"9f0v4facurakqmz4yl3crnyc8rfmgufpn8qqa2t0dk3hxs4c7f0qyrj4ej";
	assert.equal(npub, encode("npub", decode(npub)));
});
