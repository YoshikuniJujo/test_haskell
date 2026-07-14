import { createRxNostr } from "rx-nostr";
import { verifier } from "@rx-nostr/crypto";

globalThis.createRxNostr = createRxNostr;
globalThis.verifier = verifier;

import("./tryRead");
