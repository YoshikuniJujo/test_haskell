import type { Permission, Account } from "./types.js"

export class
PermissionNew
{
	#permission: Permission;
	#account: Account;

	constructor(p: Permission, a: Account)
	{
		this.#permission = p;
		this.#account = a;
	}

	permission()
	{
		return this.#permission;
	}

	account()
	{
		return this.#account;
	}
}
