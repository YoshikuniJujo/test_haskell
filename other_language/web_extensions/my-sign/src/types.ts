import { isUuid, isUrl } from "./tools.js";

export interface ClientSet { uuid: string; name: string; }

export type Client = {
	uuid: string,
	name: string, urlPattern: string,
	publicKey: Uint8Array,
	priority: number,
	openSettingsByClick: boolean,
	backgroundColor: string, backgroundOpacity: number,
	positionX: number, positionY: number,
	displayAccount: boolean }

export type ClientSummary =
	{ type: "ClientSummary", uuid: UUID, name: string, urlPattern: string }

export type ClientSummaryRaw = {
	type: "ClientSummaryRaw", uuid: string,
	name: string, urlPattern: string, priority: number }

export type EditingClient = Omit<Client, "uuid" | "publicKey" | "priority"> & {
	uuid: string | null,
	publicKey: Uint8Array | undefined,
	priority: number | null }

export function defaultEditingClient(): EditingClient
{
	return {
		uuid: null,
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

export function
uuid(s: string): UUID
{
	if (!isUuid(s)) throw new Error("Not UUID");
	return { type: "UUID", value: s };
}

export function
fromUuid(u: UUID): string
{
	return u.value;
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
		type: string;
		instance: string;
		method: "input";
		key: K;
	}
}[keyof T];

export type OptionsTabTag = { openType: string, id: string }

export function
optionsTabTag(id: string): OptionsTabTag
{
	switch (id) {
		case "browser":
		case "tab":
		case "set":
			return { openType: id, id: id };
		default:
			if (isUuid(id)) return { openType: "uuid", id: id };
			else if (isUrl(id)) return { openType: "url", id: id };
			else throw new Error(
				`optionsTabTag: Invalid options id: ${id}` );
	}
}

export type PublicKeyOption =
	{ type: "PublicKeyOption", publicKey: string, name: string }

export type Result =
	| { result: "success" } | { result: "error"; message: string }

export type Error = { result: "error"; message: string }

export function
isError(r: Result): r is Error
{
	return r.result === "error"
}

export function
success(): Result
{
	return { result: "success" };
}

export function
error(msg: string): Result
{
	return { result: "error", message: msg };
}

export type ClientResult =
	| { type: "ClientResult"; tag: "ClientSuccess"; }
	| { type: "ClientResult"; tag: "InvalidUrlPattern"; }
	| { type: "ClientResult"; tag: "UrlPatternMismatch"; }

export const ClientSuccess: ClientResult =
	{ type: "ClientResult", tag: "ClientSuccess" }

export const InvalidUrlPattern: ClientResult =
	{ type: "ClientResult", tag: "InvalidUrlPattern" }

export const UrlPatternMismatch: ClientResult =
	{ type: "ClientResult", tag: "UrlPatternMismatch" }

export function
isClientError(e: ClientResult)
{
	return isInvalidUrlPattern(e) || isUrlPatternMismatch(e);
}

export function
isInvalidUrlPattern(e: ClientResult)
{
	return e.tag === "InvalidUrlPattern";
}

export function
isUrlPatternMismatch(e: ClientResult)
{
	return e.tag === "UrlPatternMismatch";
}
