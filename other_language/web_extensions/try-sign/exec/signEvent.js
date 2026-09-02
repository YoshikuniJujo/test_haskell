import fs from 'node:fs/promises';
import { sha256 } from '@noble/hashes/sha2.js';
import { schnorr } from '@noble/secp256k1';
import * as Bech32 from '../src/codec/bech32.js';

const [skf, pkf, evf] = process.argv.slice(2);

const sk = Bech32.decode(await fs.readFile(skf, 'utf8'));
const pk = Bech32.decode(await fs.readFile(pkf, 'utf8'));
const ev = JSON.parse(await fs.readFile(evf, 'utf8'));

const pkh = Array.from(pk, b => b.toString(16).padStart(2, '0')).join('');

const evpk = { ...ev, pubkey: pkh }

const serialized = JSON.stringify([
	0, evpk.pubkey, evpk.created_at, evpk.kind, evpk.tags, evpk.content ]);

const idBytes = sha256(new TextEncoder().encode(serialized));
const auxRand = new Uint8Array(32);
const sig = await schnorr.signAsync(idBytes, sk, auxRand);
// auxRand is fixed for debugging. Remove this in production.

console.log(Buffer.from(sig).toString('hex'));

const signed = {
	...evpk,
	id: Array.from(idBytes, b => b.toString(16).padStart(2, "0")).join(""),
	sig: Buffer.from(sig).toString('hex') };

console.log(signed);
