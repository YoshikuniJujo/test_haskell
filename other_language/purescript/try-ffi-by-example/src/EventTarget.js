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

var eventTargetBody = function (b) { return b }
var eventTargetWindow = function (w) { return w }

export {
	js_addEventListenerFn,
	js_addEventListenerHhe,
	eventTargetBody, eventTargetWindow
};
