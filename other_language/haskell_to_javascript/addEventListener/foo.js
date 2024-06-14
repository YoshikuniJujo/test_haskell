const foobarbaz = 123;

const getButton = () => {
	return document.getElementById("button0");
}

const setButtonListener = (b, f) => {
	console.log("setButtonListener");
	b.addEventListener("click", (e) => f());
}

const setButtonLabel = (b, lbl) => {
	b.textContent = lbl;
}
