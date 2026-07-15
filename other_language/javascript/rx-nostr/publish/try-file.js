import { createRxNostr } from "rx-nostr";
import { seckeySigner } from "@rx-nostr/crypto";
import { readFileSync } from "fs";

const data = readFileSync('/home/tatsuya/tmp/nsec', 'utf-8');

const rxNostr = createRxNostr({
	signer: seckeySigner(data),
});

rxNostr.setDefaultRelays(["wss://r.kojira.io"]);

rxNostr.send({
	kind: 1,
	content: "Hello, from rx-nostr. 今日は2" }).subscribe((packet) => {
		console.log(`sending to ${packet.from} ${packet.ok ? "succeeded" : "failed"}.`
		);
	});
