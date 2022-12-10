import { TestCase } from './test';

export default () => [
	TestCase(
		'for',
		`fn main() {
			let x = 0;
			for (let i = 0; i < 10; i++) {
				assert(i, x++);
			}
}`
	),
	TestCase(
		'for-break',
		`fn main() {
			let x = 0;
			for (let i = 0; i < 10; i++) {
				assert(i, x++);
				if (i == 5) {
					break;
				}
			}
			assert(6, x);
}`
	),
	TestCase(
		'for-continue',
		`fn main() {
			let x = 0;
			for (let i = 0; i < 10; i++) {
				if (i >= 5) {
					continue;
				}
				assert(i, x++);
			}
			assert(5, x);
}`
	),
	TestCase(
		'while',
		`fn main() {
			let x = 0;
			while (x < 10) {
				assert(x, x++);
			}
}`
	),
	TestCase(
		'while-break',
		`fn main() {
			let x = 0;
			while (x < 10) {
				assert(x, x++);
				if (x == 5) {
					break;
				}
			}
			assert(5, x);
}`
	),
	TestCase(
		'while-continue',
		`fn main() {
			let x = 0;
			while (x < 10) {
				if (x >= 5) {
					x++;
					continue;
				}
				assert(x, x++);
			}
			assert(10, x);
}`
	),
	TestCase(
		'if',
		`fn main() {
			let x = 0;
			if (x == 0) {
				assert(0, x++);
			}
			assert(1, x);
}`
	),
	TestCase(
		'if-else',
		`fn main() {
			let x = 0;
			if (x == 1) {
				assert(true, false);
			} else {
				assert(0, x++);
			}
			assert(1, x);
}`
	),
	TestCase(
		'if-else-if',
		`fn main() {
			let x = 0;
			if (x == 1) {
				assert(true, false);
			} else if (x == 0) {
				assert(0, x++);
			} else {
				assert(true, false);
			}
			assert(1, x);
}`
	),
	TestCase(
		'if-else-if-else',
		`fn main() {
			let x = 0;
			if (x == 1) {
				assert(true, false);
			} else if (x == 2) {
				assert(true, false);
			} else {
				assert(0, x++);
			}
			assert(1, x);
}`
	)
];
