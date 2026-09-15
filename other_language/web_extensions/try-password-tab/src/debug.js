const LOG_KEY = "debug-log";

export async function
log(event, data = {})
{
	const { [LOG_KEY]: log = [] } =
		await browser.storage.local.get(LOG_KEY);

	log.push({
		time: Date.now(),
		event,
		...data
	});

	await browser.storage.local.set({
		[LOG_KEY]: log
	});
}
