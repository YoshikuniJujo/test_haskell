const debugLog = document.querySelector("#debug-log");

function
displayLog(log)
{
	debugLog.textContent = log.map(formatLog).join("\n");
}

async function
loadLog()
{
//	const { debug-log: log = [] } = await browser.storage.local.get("debug-log");
	const data = await browser.storage.local.get("debug-log");
	const log = data["debug-log"] ?? [];
	console.log(log);
	displayLog(log);
}

function
formatLog(entry)
{
	const time = new Date(entry.time)
		.toLocaleString();
	const data = Object.entries(entry)
		.filter(([key]) => key !== "time")
		. map(([key, value]) => `${key}=${value}` ).join(" ");
	return `${time} ${data}`;
}

browser.storage.onChanged.addListener(
	(changes, area) => {
		if (area !== "local") return;
		const change = changes["debug-log"];
		if (!change) return;
		displayLog(change.newValue ?? []);
	});

loadLog();

document.querySelector("#clear-log").addEventListener("click", async () => {
	await browser.storage.local.remove("debug-log");
});
