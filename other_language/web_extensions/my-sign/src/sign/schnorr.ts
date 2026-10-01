import { sha256 } from '@noble/hashes/sha2.js';
import { schnorr } from '@noble/secp256k1';

type Event = {
	created_at: number,
	kind: number,
	tags: string[][],
	content: string
}

export async function
signEvent(ev: Event, sk: Uint8Array, pk: Uint8Array)
{
	const pkh = hex(pk);
	const srlzd = JSON.stringify(
		[0, pkh, ev.created_at, ev.kind, ev.tags, ev.content] );
	const id = sha256(new TextEncoder().encode(srlzd));
	return { ...ev, pubkey: pkh, id: hex(id),
		sig: hex(await schnorr.signAsync(id, sk, undefined)) };
}

function
hex(bs: Uint8Array)
{
	return Array.from(bs, b => b.toString(16).padStart(2, "0")).join("");
}
