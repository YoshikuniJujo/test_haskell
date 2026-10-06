export type Client = {
	uuid: string,
	name: string, urlPattern: string,
	publicKey: Uint8Array,
	priority: number,
	openSettingsByClick: boolean
	backgroundColor: string, backgroundOpacity: number,
	positionX: number, positionY: number,
	displayAccount: boolean }

export type EditingClient = Omit<Client, "uuid" | "publicKey" | "priority"> & {
	publicKey: Uint8Array | undefined,
	priority: number | null }

export function defaultEditingClient()
{
	return {
		name: "", urlPattern: "",
		publicKey: undefined,
		priority: 100,
		openSettingsByClick: true,
		backgroundColor: "#008000", backgroundOpacity: 0.5,
		positionX: 100, positionY: 0,
		displayAccount: true };
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

export type UUID = { type: "UUID", value: string };

function
uuid(s: string): UUID
{
	return { type: "UUID", value: s };
}

export function
uuidNull(s: string | null)
{
	return s === null ? null : uuid(s);
}

export type OptionsInputMessages<T> =
{
	[K in keyof T]: {
		class: "Options";
		instance: string;
		method: "input";
		key: K;
	}
}[keyof T];
