import { sha256 } from '@noble/hashes/sha2.js';
import { schnorr } from '@noble/secp256k1';

export async function
signEvent(ev, sk, pk)
{
	const pkh = hex(pk);
	const srlzd = JSON.stringify(
		[0, pkh, ev.created_at, ev.kind, ev.tags, ev.content] );
	const id = sha256(new TextEncoder().encode(srlzd));
	return { ...ev, pubkey: pkh, id: hex(id),
		sig: hex(await schnorr.signAsync(id, sk, undefined)) };
}

function
hex(bs)
{
	return Array.from(bs, b => b.toString(16).padStart(2, "0")).join("");
}
