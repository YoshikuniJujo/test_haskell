async function foo() {
	const checkForDebug = document.getElementById('debug');

	const ckd = await browser.storage.local.get("slothDebug");
	checkForDebug.checked = ckd;

	checkForDebug.addEventListener('change', async (event) => {
		if (event.target.checked) {
			await browser.storage.local.set({ slothDebug: true });
		} else {
			await browser.storage.local.set({ slothDebug: false });
		}
	});
}

foo();
