async function foo() {
const checkForDebug = document.getElementById('debug');
checkForDebug.addEventListener('change', async (event) => {
	if (event.target.checked) {
		await browser.storage.local.set({ slothDebug: true });
	} else {
		await browser.storage.local.set({ slothDebug: false });
	}
});
}

foo();
