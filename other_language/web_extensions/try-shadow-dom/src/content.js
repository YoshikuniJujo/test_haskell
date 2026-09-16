document.documentElement.style.border = "5px solid green";

(async () => {
	const div = document.createElement("div");

	Object.assign(div.style, {
		position: "fixed",
		padding: "8px 12px",
		background: "rgba(0, 123, 0, 0.5)",
		color: "white",
		whiteSpace: "nowrap",
		zIndex: "2147483647"
	});

	const adi = await browser.runtime.sendMessage({
		method: "accountDisplayInfo" });

	console.log("content:", adi);

	document.body.append(div);
	div.textContent = "Foo Bar";

	setAccountPosition(div, adi);
})();

function
setAccountPosition(div, adi)
{
	const rect = div.getBoundingClientRect();
	const left = (innerWidth - rect.width) * adi.positionX / 100;
	const top = (innerHeight - rect.height) * adi.positionY / 100;
	console.log(left, top);
	div.style.left = `${left}px`
	div.style.top = `${top}px`
}
