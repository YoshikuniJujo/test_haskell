const result = document.getElementById("result");

document.querySelector("#open-input").addEventListener("click", async () => {

	result.textContent = "入力待ち...";

	try {
		const value = await window.test.openInputTab();
		result.textContent = `結果: ${value}`;
	} catch (e) {
		result.textContent = `エラー: ${e}`;
	}

});

