import { writable } from 'svelte/store';
import all from 'src/tests/all';

export const tester = writable({
	cases: all
});
