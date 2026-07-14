import { createRxNostr, createRxForwardReq } from "rx-nostr";
import { verifier } from "@rx-nostr/crypto";

globalThis.createRxNostr = createRxNostr;
globalThis.createRxForwardReq = createRxForwardReq;
globalThis.verifier = verifier;

import("./tryRead");
