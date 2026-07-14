async function generateKeyFromPassword(password, salt) {
	const encoder = new TextEncoder();

	const passwordKey = await globalThis.crypto.subtle.importKey(
		"raw", encoder.encode(password),
		{ name: "PBKDF2" }, false, ["deriveKey"] );
	console.log(passwordKey);

	const aesKey = await globalThis.crypto.subtle.deriveKey(
		{	name: "PBKDF2", salt: salt,
			iterations: 100000, hash: "SHA-256" },
		passwordKey,
		{	name: "AES-GCM",
			length: 256 },
		true, ["encrypt", "decrypt"] );
	console.log(aesKey);

	return aesKey;
}

async function encryptData(aesKey, plaintext) {
	const encoder = new TextEncoder();
	const dataBuffer = encoder.encode(plaintext);

	const iv = globalThis.crypto.getRandomValues(new Uint8Array(12));

	const encryptedBuffer = await globalThis.crypto.subtle.encrypt(
		{	name: "AES-GCM",
			iv: iv },
		aesKey, dataBuffer );

	return {
		cipherText: new Uint8Array(encryptedBuffer),
		iv: iv };
}

async function decryptData(aesKey, cipherText, iv) {
	try {
		const decryptedBuffer = await globalThis.crypto.subtle.decrypt(
			{	name: "AES-GCM", iv: iv },
			aesKey, cipherText );
		const decoder = new TextDecoder();
		return decoder.decode(decryptedBuffer);
	} catch (e) {
		throw new Error("複号に失敗したし");
	}
}

const salt = globalThis.crypto.getRandomValues(new Uint8Array(16));

const aesKey = await generateKeyFromPassword("foo", salt);

const encryptedResult = await encryptData(aesKey, "私は私とはぐれるわけには(いく/いかない)?");

console.log(encryptedResult);

const decryptedMessage = await decryptData(
	aesKey, encryptedResult.cipherText, encryptedResult.iv );

console.log(decryptedMessage);
