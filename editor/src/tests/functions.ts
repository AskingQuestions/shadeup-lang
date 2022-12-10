import { TestCase } from './test';

export default () => [
	TestCase(
		'function',
		`
fn test() -> int {
	return 1;
}

fn main() {
	assert(1, test());
}`
	),
	TestCase(
		'function-args',
		`
fn sum(a: int, b: int) -> int {
	return a + b;
}

fn main() {
	assert(3, sum(sum(1, 1), 1));
}`
	),
	TestCase(
		'function-recursion',
		`
fn fib(n: int) -> int {
	if (n <= 1) {
		return n;
	}
	return fib(n - 1) + fib(n - 2);
}

fn main() {
	assert(1, fib(2));
	assert(2, fib(3));
	assert(3, fib(4));
	assert(5, fib(5));
	assert(8, fib(6));
	assert(13, fib(7));
	assert(21, fib(8));
	assert(34, fib(9));
	assert(55, fib(10));
}`
	),
	TestCase(
		'sleep',
		`
fn test() {
	print("Starting");
	sleep(2);
	assert(1, 1);
	sleep(2);
	assert(2, 2);
	sleep(4);
	assert(4, 4);
	print("Ending");
}

fn main() {
	test();
}
`
	)
];
