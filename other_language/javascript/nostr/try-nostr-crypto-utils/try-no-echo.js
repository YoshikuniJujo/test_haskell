import { execFileSync } from 'node:child_process';

console.log(process.stdin.isTTY);

process.stdout.write('Password: ');
const password = await readPassword();
console.log(password);

async function readPassword() {

	execFileSync('stty', ['-echo'], {
		stdio: ['inherit', 'inherit', 'inherit']
	});

	try {
		const chunks = [];

		for await (const chunk of process.stdin) {
			chunks.push(chunk);
			if(chunk.toString().includes('\n')) {
				break;
			}
		}

		return Buffer.concat(chunks).toString().trimEnd();

	} finally {
		execFileSync('stty', ['echo'], {
			stdio: ['inherit', 'inherit', 'inherit']
		});
	}
}


