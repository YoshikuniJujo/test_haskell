export interface Storage<T> {
	get(key: string): Promise<Record<string, T>>;
	set(items: Record<string, T>): Promise<void>;
}
