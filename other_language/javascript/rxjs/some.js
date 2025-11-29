var observer_1 = {
	next: x => console.log('Observer1 got a value: ' + x),
	error: err => console.log('Observer1 got an error: ' + err),
	complete: () => console.log('Observer1 got Complete'),
};

function subscribe_1(observer) {
	observer.next(1);
	observer.next(2);
	observer.next(3);
	observer.complete();
};

subscribe_1(observer_1);
