import { writable } from 'svelte/store';
import all from 'src/tests/all';

export const tester = writable({
	cases: all()
});

export function resetCases() {
	tester.update((t) => {
		t.cases = all();

		return t;
	});
}
