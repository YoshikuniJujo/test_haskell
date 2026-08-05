import * as readline from 'node:readline/promises';
import { stdin as input, stdout as output } from 'node:process';
import { execFileSync } from 'node:child_process';

const rl = readline.createInterface({ input, output });

let password;

try {
	execFileSync('stty', ['-echo'], {
		stdio: ['inherit', 'inherit', 'inherit']
	});

	password = await rl.question('Password: ');

} finally {
	execFileSync('stty', ['echo'], {
		stdio: ['inherit', 'inherit', 'inherit']
	});
	rl.close();
//	process.stdout.write('\n');
}

console.log(password);
