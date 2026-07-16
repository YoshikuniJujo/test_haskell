console.log("BORDERIFY");
alert("HELLO");
document.body.style.border = "5px solid red";

chrome.runtime.onInstalled.addListener(() => {
	console.log("Hello, world!");
});
