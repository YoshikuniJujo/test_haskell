import type { ClientSummary, PublicKeyOption, UUID } from "./types.js";

export type OptionsObject = {
	accountName: string,
	showPassword: boolean,
	passwordErrorHidden: boolean,

	clientsHidden: boolean,
	newClientButtonHidden: boolean,
	deleteClientButtonHidden: boolean,
	clientDetailHidden: boolean

	clients: ClientSummary[],

	clientUuid: UUID | null,
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

		clientUuid: null,
		clientName: "",
		urlPattern: "",
		usePriority: false,
		priority: 100,
		displayAccount: true,
		positionX: 100,
		positionY: 0,
		backgroundColor: "#008000",
		backgroundOpacity: 0.5,
		openSettingsByClick: true,
		currentKey: null,
		currentKeyOptions: [],
		detailError: "",

		optionsError: "",
		useClientSet: false };
}

export function
newClientMode(obj: OptionsObject)
{
	obj.deleteClientButtonHidden = true;
	obj.clientsHidden = true;
	obj.newClientButtonHidden = true;
	obj.clientDetailHidden = false;
	obj.optionsError = "";
}
