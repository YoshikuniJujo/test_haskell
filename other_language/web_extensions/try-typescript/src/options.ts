console.log("foobar");

const foobar = document.querySelector("#foobar");

if (!(foobar instanceof HTMLElement))
	throw new Error("#foobar not found");

foobar.addEventListener("click", (e: MouseEvent) => {
	console.log(e);
});

const tryMessage = document.querySelector("#try-message");

if (!(tryMessage instanceof HTMLElement))
	throw new Error("#try-message not found");

tryMessage.addEventListener("click", async e => {
	console.log(e);
	const foo = await browser.runtime.sendMessage({ method: "foobar" });
	console.log(foo);
});
