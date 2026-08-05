import readlineSync from 'readline-sync';

const password = readlineSync.question('Password: ', {
	hideEchoBack: true,
	mask: ''
});

console.log('入力されたパスワード: ', password);
