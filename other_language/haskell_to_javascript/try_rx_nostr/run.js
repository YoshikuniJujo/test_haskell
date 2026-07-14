import { createRxNostr, createRxForwardReq } from "https://esm.sh/rx-nostr";
import { verifier } from "https://esm.sh/rx-nostr-crypto";

globalThis.createRxNostr = createRxNostr;
globalThis.createRxForwardReq = createRxForwardReq;
globalThis.verifier = verifier;

import("./tryRead.jsexe/all.js");
