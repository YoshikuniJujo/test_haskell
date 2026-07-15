import { createRxNostr } from "rx-nostr";
import { seckeySigner } from "@rx-nostr/crypto";
import { readFileSync } from "fs";

const data = readFileSync('/home/tatsuya/tmp/nsec', 'utf-8');

const rxNostr = createRxNostr({
	signer: seckeySigner(data),
});

rxNostr.setDefaultRelays(["wss://r.kojira.io"]);

const message = process.argv[2];

rxNostr.send({
	kind: 1,
	content: message }).subscribe({
		next: (packet) => {
			console.log(`sending to ${packet.from} ${packet.ok ? "succeeded" : "failed"}.`); },
		complete: () => {
			console.log("終了");
			rxNostr.dispose(); }
	});
