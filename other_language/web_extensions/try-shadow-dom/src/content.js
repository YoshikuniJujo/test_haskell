import { getAccountDisplayInfo } from "./db.js"

document.documentElement.style.border = "5px solid green";

const div = document.createElement("div");

(async () => {
	const adi = await getAccountDisplayInfo();
	console.log(adi);

	Object.assign(div.style, {
		position: "fixed",
		padding: "8px 12px",
		background: "rgba(0, 123, 0, 0.5)",
		color: "white",
		whiteSpace: "nowrap",
		zIndex: "2147483647"
	});

	document.body.append(div);
	div.textContent = "Foo Bar";
})();
