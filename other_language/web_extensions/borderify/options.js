const checkForDebug = document.getElementById('debug');
checkForDebug.addEventListener('change', (event) => {
	if (event.target.checked) {
		browser.storage.local.set( { slothDebug: true }, () => { });
	} else {
		browser.storage.local.set( { slothDebug: false }, () => { });
	}
});
