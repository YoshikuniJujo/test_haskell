var js_getElementTypeStrById = function (id) { return function() {
	const e = document.getElementById(id);
	if (e === null) { return "NULL"; } else { return e.tagName; }
}; };

export {
	js_getElementTypeStrById,
};

