import type { UUID } from "./types.js";

export type OptionsObject = {
	accountName: string,
	showPassword: boolean,
	passwordErrorHidden: boolean,

	clientsHidden: boolean,
	newClientButtonHidden: boolean,
	deleteClientButtonHidden: boolean,
	clientDetailHidden: boolean

	clients: ClientSummary[],

	clientName: string,
	urlPattern: string,
	usePriority: boolean,
	priority: number,
	displayAccount: boolean,
	positionX: number,
	positionY: number,
	backgroundColor: string,
	backgroundOpacity: number,
	openSettingsByClick: boolean,
	currentKey: string | null,
	currentKeyOptions: PublicKeyOption[],
	detailError: string,

	optionsError: string,
	useClientSet: boolean
}

export type ClientSummary =
	{ type: "ClientSummary", uuid: UUID, name: string, urlPattern: string }

export type PublicKeyOption =
	{ type: "PublicKeyOption", publicKey: string, name: string }

export function defaultOptionsObject(): OptionsObject
{
	return {
		accountName: "",
		showPassword: false,
		passwordErrorHidden: true,

		clientsHidden: false,
		newClientButtonHidden: false,
		deleteClientButtonHidden: true,
		clientDetailHidden: true,

		clients: [],

		clientName: "",
		urlPattern: "",
		usePriority: false,
		priority: 100,
		displayAccount: true,
		positionX: 100,
		positionY: 0,
		backgroundColor: "#00ff00",
		backgroundOpacity: 0.5,
		openSettingsByClick: true,
		currentKey: null,
		currentKeyOptions: [],
		detailError: "",

		optionsError: "",
		useClientSet: false };
}
