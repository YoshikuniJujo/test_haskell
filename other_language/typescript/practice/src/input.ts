import { createInterface } from 'readline';

const rl = createInterface({ input: process.stdin, output: process.stdout });

rl.question("文字を入力してください:", (line) => {
	console.log(`${line}が入力されました`)
	rl.close(); });
