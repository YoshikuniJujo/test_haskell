var unsafeHead = function(arr) {
	if (arr.length) {
		return arr[0];
	} else {
		throw new Error('unsafeHead: empty array');
	}
}

var head = function(arr) {
	return arr[0];
}

var isUndefined = function(value) {
	return value === undefined;
}

export {
	unsafeHead,
	head,
	isUndefined
}
