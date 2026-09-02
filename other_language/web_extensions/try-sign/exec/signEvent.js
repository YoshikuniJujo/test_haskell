import fs from 'node:fs/promises';
import * as Bech32 from '../src/codec/bech32.js';
import { signEvent } from '../src/crypto/shnorr.js';

const [skf, pkf, evf] = process.argv.slice(2);

const sk = Bech32.decode(await fs.readFile(skf, 'utf8'));
const pk = Bech32.decode(await fs.readFile(pkf, 'utf8'));
const ev = JSON.parse(await fs.readFile(evf, 'utf8'));

console.log(await signEvent(ev, sk, pk));
