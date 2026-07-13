export default function logging(foo) {
	console.log("Hello, module world!");
	console.log(foo);
}

global.GlobalLog = function(foo) {
	console.log(foo);
}
