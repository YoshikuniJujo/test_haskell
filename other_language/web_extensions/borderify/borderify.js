function updateSloth(sloth, slothSize, reverse, debug) {
	const width2 = (window.innerWidth - slothSize) / 2;
	const now = new Date();
	const som = debug ? now.getSeconds() : now.getMinutes();
	phase = som / 60 * 2 * 3.14;

	sloth.style.left = `${width2 + width2 * Math.sin(phase)}px`;
	sloth.style.transform = `scaleX(${Math.sign(Math.cos(phase)) * reverse})`
}

alert("foobar");

browser.storage.local.get("slothDebug", (items) => {

	alert("foobarbaz");

	alert(items.slothDebug);
const debug = items.slothDebug;

const platform = navigator.userAgent.toLowerCase();
const reverse = platform.indexOf('android') == -1 ? 1 : - 1;

console.log("BORDERIFY");
document.body.style.border = "5px solid red";

const slothSize = 48
const sloth = document.createElement("div");
sloth.textContent = "🦥";
sloth.style.position = "fixed";
sloth.style.top = "0";
sloth.style.fontSize = `${slothSize}px`;
sloth.style.zIndex = "2147483647";
sloth.style.pointerEvents = "none";

updateSloth(sloth, slothSize, reverse, debug);
	alert("after");

document.body.appendChild(sloth);

setInterval(() => updateSloth(sloth, slothSize, reverse, debug), debug ? 1_000 : 60_000);
});
