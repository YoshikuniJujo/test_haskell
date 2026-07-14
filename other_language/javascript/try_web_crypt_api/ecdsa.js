async function demoECDSA() {
	const keyPair = await globalThis.crypto.subtle.generateKey(
		{	name: "ECDSA",
			namedCurve: "P-256" },
		true, ["sign", "verify"] );
	console.log(keyPair);

	const encoder = new TextEncoder();
	const data = encoder.encode("われ泣きぬれて蟹みそ食べる");

	const signature = await globalThis.crypto.subtle.sign(
		{	name: "ECDSA",
			hash: { name: "SHA-256" } },
		keyPair.privateKey, data );

	console.log(new Uint8Array(signature));

	const isValid = await globalThis.crypto.subtle.verify(
		{	name: "ECDSA",
			hash: { name: "SHA-256" } },
		keyPair.publicKey, signature, data );

	console.log(isValid);
}

demoECDSA();
