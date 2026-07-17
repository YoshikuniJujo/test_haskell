const checkForDebug = document.getElementById('debug');
checkForDebug.addEventListener('change', (event) => {
	if (event.target.checked) {
		chrome.storage.local.set( { slothDebug: true }, () => { });
	} else {
		chrome.storage.local.set( { slothDebug: false }, () => { });
	}
});
