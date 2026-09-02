import { sha256 } from '@noble/hashes/sha2.js';
import { schnorr } from '@noble/secp256k1';

const FIXED_AUX_RAND = true;
// const FIXED_AUX_RAND = false;
// auxRand is fixed for reproducible signatures.

export async function
signEvent(ev, sk, pk)
{
	const pkh =
		Array.from(pk, b => b.toString(16).padStart(2, '0')).join('');
	const evpk = { ...ev, pubkey: pkh }
	const serialized = JSON.stringify([
		0, evpk.pubkey, evpk.created_at,
		evpk.kind, evpk.tags, evpk.content ]);
	const id = sha256(new TextEncoder().encode(serialized));
	const auxRand = FIXED_AUX_RAND ? new Uint8Array(32) : undefined;
	const sig = await schnorr.signAsync(id, sk, auxRand);
	return {
		...evpk,
		id: Array.from(
			id, b => b.toString(16).padStart(2, "0") ).join(""),
		sig: Array.from(
			sig, b => b.toString(16).padStart(2, "0") ).join("") };
}
