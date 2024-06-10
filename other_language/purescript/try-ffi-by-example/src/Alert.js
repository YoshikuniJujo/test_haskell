var alert = function (s) {
	return function() {
		window.alert(s);
	}
};

export {
	alert
};
