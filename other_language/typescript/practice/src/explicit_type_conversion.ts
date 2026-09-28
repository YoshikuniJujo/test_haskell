import { createInterface } from 'readline';

const rl = createInterface({ input: process.stdin, output: process.stdout });

rl.question("数字を入力してください:", (line) => {
	// 1000を足して出力
	const num = Number(line);
	console.log(num + 1000);
	rl.close(); });
