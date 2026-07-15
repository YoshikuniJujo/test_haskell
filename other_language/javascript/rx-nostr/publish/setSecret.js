console.log("我は行く");
localStorage.setItem("noskell:seckey", "hogehige");
console.log(localStorage.getItem("noskell:seckey"));

const passwordInput = document.getElementById('password-input');
const toggleBtn = document.getElementById('toggle-btn');

toggleBtn.addEventListener('click', () => {
	if (passwordInput.type == 'password') {
		passwordInput.type = 'text';
		toggleBtn.textContent = '見ない';
	} else {
		passwordInput.type ='password';
		toggleBtn.textContent = '見る';
	} })
