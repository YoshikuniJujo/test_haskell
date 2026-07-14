const rxNostr = createRxNostr({ verifier });
rxNostr.setDefaultRelays([
	"wss:yabu.me"
]);

const rxReq = createRxForwardReq();

const subscription = rxNostr.use(rxReq).subscribe((packet) => {
	console.log(packet);
});

rxReq.emit({ kinds: [1] });

setTimeout(() => {
	subscription.unsubscribe();
}, 2 * 1000);
