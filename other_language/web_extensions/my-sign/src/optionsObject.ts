import type { ClientSummary, PublicKeyOption, UUID } from "./types.js";

export type OptionsObject = {
	accountName: string,

	clientsHidden: boolean,
	newClientButtonHidden: boolean,
	deleteClientButtonHidden: boolean,
	clientDetailHidden: boolean,

	revertButtonHidden: boolean,
	applyButtonHidden: boolean,
	cancelButtonHidden: boolean,

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

	openInTabButtonHidden: boolean,
	optionsError: string,
	useClientSet: boolean,
	selectSetMode: boolean
}

export function defaultOptionsObject(): OptionsObject
{
	return {
		accountName: "",

		clientsHidden: false,
		newClientButtonHidden: false,
		deleteClientButtonHidden: true,
		clientDetailHidden: true,

		revertButtonHidden: false,
		applyButtonHidden: false,
		cancelButtonHidden: false,

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

		openInTabButtonHidden: false,
		optionsError: "",
		useClientSet: false,
		selectSetMode: false
	};
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

export function
clientDetailMode(obj: OptionsObject)
{
	obj.deleteClientButtonHidden = false;
	obj.clientsHidden = true;
	obj.newClientButtonHidden = true;
	obj.clientDetailHidden = false;
	obj.optionsError = "";
}

export function
clientListMode(obj: OptionsObject)
{
	obj.deleteClientButtonHidden = true;
	obj.clientsHidden = false;
	obj.newClientButtonHidden = false;
	obj.clientDetailHidden = true;
	obj.optionsError = "";
}
