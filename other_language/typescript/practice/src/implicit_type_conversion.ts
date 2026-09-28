import { createInterface } from 'readline';

const rl = createInterface({ input: process.stdin, output: process.stdout });

rl.question("文字を入力してください:", (line) => {
	// 1000を足して出力
	console.log(line + 1000);
	rl.close(); });
