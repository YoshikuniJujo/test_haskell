var js_addEventListener = function (evtg) {
	return function (evtp) {
		return function (opts) {
			return function (cb) {
				return function () {
					console.log(evtg);
					console.log(evtp);
					console.log(opts);
					console.log(cb);
					evtg.addEventListener(evtp, function (ev) { cb(ev)(); console.log("foobarbaz"); }, opts);
//					window.addEventListener("load", function (ev) { console.log("baaaa"); });
				}
			}
		}
	}
};

var js_addEventListenerFn = js_addEventListener
var js_addEventListenerHhe = js_addEventListener

var js_addEventListenerFnNoSignal = js_addEventListener
var js_addEventListenerHheNoSignal = js_addEventListener

var eventTargetBody = function (b) { return b }
var eventTargetWindow = function (w) { return w }

var js_fromEvent = function (e) { return e }

var js_currentTarget = function (e) { return e.currentTarget }

export {
	js_addEventListenerFn,
	js_addEventListenerHhe,
	js_addEventListenerFnNoSignal,
	js_addEventListenerHheNoSignal,
	eventTargetBody, eventTargetWindow,
	js_fromEvent,
	js_currentTarget
};
