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
	currentKey: UUID | null,
	detailError: string,

	optionsError: string,
	useClientSet: boolean
}

export type ClientSummary =
	{ type: "ClientSummary", uuid: UUID, name: string, urlPattern: string }

export type UUID = { type: "UUID", value: string };
