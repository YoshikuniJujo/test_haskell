import { readPassword } from "./readPassword.js";

const [cmd, uid] = process.argv.slice(2);
const pswd = await readPassword();

const response = await fetch(
	`http://localhost:3000/${cmd}`,
	{	method: "POST",
		headers: { "Content-Type": "application/json" },
		body: JSON.stringify({ uid, pswd }) } );
console.log(await response.json());
