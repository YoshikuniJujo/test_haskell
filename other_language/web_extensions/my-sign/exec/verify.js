import { sha256 } from "@noble/hashes/sha2.js";
import { schnorr } from "@noble/secp256k1";

export async function
verifyEvent(signed)
{
	const srlzd = JSON.stringify( [
		0, signed.pubkey, signed.created_at, signed.kind,
		signed.tags, signed.content] );
	const hash = sha256(new TextEncoder().encode(srlzd));
	console.log(Array.from(hash, b => b.toString(16).padStart(2, "0")).join(""));
	console.log(signed.id);
	console.log(await schnorr.verifyAsync(
		hexToBytes(signed.sig), hash, hexToBytes(signed.pubkey) ));
}

// HEXES <=> BYTES

function
hexToBytes(hex)
{
	const bs = new Uint8Array(hex.length / 2);
	for (let i = 0; i < bs.length; i++)
		bs[i] = parseInt(hex.slice(i * 2, i * 2 + 2), 16);
	return bs
}
