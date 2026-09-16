import { putAccountDisplayInfo, listAccountDisplayInfos } from "./db.js"

const form = document.querySelector("#form");

const positionX = document.querySelector("#position-x");
const positionY = document.querySelector("#position-y");

form.addEventListener("submit", async event => {

	event.preventDefault();

	console.log("submit");
	const adi = {
		uuid: "0123456789",
		positionX: positionX.value,
		positionY: positionY.value
	};
	console.log("options.js: before put");
	await putAccountDisplayInfo(adi);
	console.log("options.js: after put");
	console.log(await listAccountDisplayInfos());
});
