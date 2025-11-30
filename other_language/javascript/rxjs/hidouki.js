import { of } from 'rxjs';

var observer_1 = {
	next: x => console.log('Observer1 got a value: ' + x),
	error: err => console.log('Observer1 got an error: ' + err),
	complete: () => console.log('Observer1 got Complete'),
};

var observable_1 = of(1, 2, 3);

observable_1.subscribe(observer_1);
