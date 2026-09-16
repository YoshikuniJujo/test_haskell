document.documentElement.style.border = "5px solid green";

const div = document.createElement("div");
let position

(async () => {
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

	position = { x: adi.positionX, y: adi.positionY };
	setPosition();
})();

function
setPosition()
{
	const rect = div.getBoundingClientRect();
	const left = (innerWidth - rect.width) * position.x / 100;
	const top = (innerHeight - rect.height) * position.y / 100;
	console.log(left, top);
	div.style.left = `${left}px`
	div.style.top = `${top}px`
}

window.addEventListener("resize", () => { setPosition(); });
