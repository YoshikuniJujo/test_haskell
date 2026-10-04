export type Client = {
	uuid: string,
	name: string,
	urlPattern: string,
	publicKey: Uint8Array,
	priority: number,
	openSettingsByClick: boolean
	backgroundColor: string, backgroundOpacity: number,
	positionX: number, positionY: number,
	displayAccount: boolean,
}

export type Account = {
	publicKey: Uint8Array ,
	name: string,
	logN: number,
	salt: Uint8Array,
	nonce: Uint8Array,
	keySecurityByte: number,
	ciphertext: Uint8Array,
	saltForCheckPassword: Uint8Array,
	hashForCheckPassword: Uint8Array }
