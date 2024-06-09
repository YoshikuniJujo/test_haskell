var app = function(f) {
	return function(x) {
		return f(x);
	};
};

var appEff0 = function(a) {
	return function(x) {
		console.log("appEff0");
		return a(x);
	};
};

var appEff = function(a) {
	return function(x) {
		return function () {
			console.log("appEff");
			return a(x)();
		};
	};
};

export {
	app, appEff0, appEff
};
