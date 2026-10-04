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
//	currentKeyOptions:
	detailError: string,

	optionsError: string,
	useClientSet: boolean
}

export type ClientSummary =
	{ type: "ClientSummary", uuid: UUID, name: string, urlPattern: string }

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
		detailError: "",

		optionsError: "",
		useClientSet: false };
}
