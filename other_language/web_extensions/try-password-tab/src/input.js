const requestId = new URLSearchParams(location.search).get("request");
const input = document.querySelector("#input");
input.focus();

input.addEventListener("keydown", (event) => {
	if (event.key === "Enter") {
		event.preventDefault();
		returnInput(requestId, input.value); } });

document.querySelector("#send").addEventListener("click", () => {
	returnInput(requestId, input.value); });

function
returnInput(rid, v)
{
	browser.runtime.sendMessage(
		{method: "returnInput", request: rid, value: v} );
}
