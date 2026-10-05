export function
getElement<T extends Element>(selector: string): T
{
	const element = document.querySelector<T>(selector);
	if (!element) throw new Error(`Element not found: ${selector}`);
	return element
}
