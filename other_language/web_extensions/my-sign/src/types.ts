export type Client = {
	uuid: string,
	name: string,
	displayAccount: boolean,
	publicKey: Uint8Array,
	positionX: number, positionY: number,
	backgroundColor: string, backgroundOpacity: number,
	priority: number,
	urlPattern: string,
	openSettingsByClick: boolean
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
