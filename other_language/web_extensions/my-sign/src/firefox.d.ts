declare function cloneInto<T>(
	obj: T,
	targetScope: object,
	option?: object
); T;

interface Window {
	wrappedJSObject: window;
}
