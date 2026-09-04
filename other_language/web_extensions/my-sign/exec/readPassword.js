import { stdin, stdout } from "node:process";

export async function
readPassword(prompt = "Password: ")
{
	stdout.write(prompt);
	stdin.setRawMode(true);
	stdin.resume();
	stdin.setEncoding("utf8");
	let pswd = "";
	return await new Promise((rslv) => {
		stdin.on("data", function onData(ky) {
			switch (ky) {
				case "\r":
				case "\n":
					stdin.setRawMode(false); stdin.pause();
					stdin.off("data", onData);
					stdout.write("\n"); rslv(pswd); break;
				case "\u0003":
					stdin.setRawMode(false); stdin.pause();
					stdin.off("data", onData);
					process.exit(); break;
				case "\u007f":
				case "\b":
					pswd = pswd.slice(0, -1); break;
				default:
					pswd += ky; break; } }); });
}
