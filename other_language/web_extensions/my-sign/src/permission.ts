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

	setPermission(p: "none" | "publicKey" | "sign")
	{
		switch (p) {
			case "none": {
				this.#permission.getPublicKey = false;
				this.#permission.sign = false;
				this.#permission.encrypt = false;
				this.#permission.decrypt = false;
				break; }
			case "publicKey": {
				this.#permission.getPublicKey = true;
				this.#permission.sign = false;
				this.#permission.encrypt = false;
				this.#permission.decrypt = false;
				break; }
			case "sign": {
				this.#permission.getPublicKey = true;
				this.#permission.sign = true;
				this.#permission.encrypt = true;
				this.#permission.decrypt = false;
				break; }
		}
	}
}
