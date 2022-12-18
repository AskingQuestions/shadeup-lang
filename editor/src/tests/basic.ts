import { TestCase } from './test';

export default () => [
	TestCase(
		'sum',
		`fn main() {
	assert(0 + 0, 0);
	assert(1 + 0, 1);
	assert(1 + 2, 3);
	assert(100 + 1, 101);
	assert(241241512 + 241241512 + 241241512, 723724536);
	assert(1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10, 55);
	assert(1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 + 18 + 19 + 20, 210);
	assert(1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 + 18 + 19 + 20 + 21 + 22 + 23 + 24 + 25 + 26 + 27 + 28 + 29 + 30, 465);
}`
	),
	TestCase(
		'sum with negatives',
		`fn main() {
	assert(0 + -1, -1);
	assert(-1 + 0, -1);
	assert(-1 + -1, -2);
	assert(1 + -1, 0);
	assert(-1 + 1, 0);
	assert(1 + -2, -1);
	assert(-2 + 1, -1);
	assert(1 + -3, -2);
	assert(-3 + 1, -2);
	assert(1 + -4 + -10, -13);
	assert(-1 + -2 + -3 + -4 + -5 + -6 + -7 + -8 + -9 + -10, -55);
	assert(-1 + -2 + -3 + -4 + -5 + -6 + -7 + -8 + -9 + -10 + -11 + -12 + -13 + -14 + -15 + -16 + -17 + -18 + -19 + -20, -210);
	assert(-1 + -2 + -3 + -4 + -5 + -6 + -7 + -8 + -9 + -10 + -11 + -12 + -13 + -14 + -15 + -16 + -17 + -18 + -19 + -20 + -21 + -22 + -23 + -24 + -25 + -26 + -27 + -28 + -29 + -30, -465);
}`
	),
	TestCase(
		'sub',
		`fn main() {
	assert(0 - 0, 0);
	assert(1 - 0, 1);
	assert(1 - 2, -1);
	assert(100 - 1, 99);
	assert(241241512 - 241241512 - 241241512, -241241512);
	assert(1 - 2 - 3 - 4 - 5 - 6 - 7 - 8 - 9 - 10, -53);
	assert(1 - 2 - 3 - 4 - 5 - 6 - 7 - 8 - 9 - 10 - 11 - 12 - 13 - 14 - 15 - 16 - 17 - 18 - 19 - 20, -208);
	assert(1 - 2 - 3 - 4 - 5 - 6 - 7 - 8 - 9 - 10 - 11 - 12 - 13 - 14 - 15 - 16 - 17 - 18 - 19 - 20 - 21 - 22 - 23 - 24 - 25 - 26 - 27 - 28 - 29 - 30, -463);
}`
	),
	TestCase(
		'sub with negatives',
		`fn main() {
	assert(0 - -1, 1);
	assert(-1 - 0, -1);
	assert(-1 - -1, 0);
	assert(1 - -1, 2);
	assert(-1 - 1, -2);
	assert(1 - -2, 3);
	assert(-2 - 1, -3);
	assert(1 - -3, 4);
	assert(-3 - 1, -4);
	assert(1 - -4 - -10, 15);
	assert(-1 - -2 - -3 - -4 - -5 - -6 - -7 - -8 - -9 - -10, 53);
	assert(-1 - -2 - -3 - -4 - -5 - -6 - -7 - -8 - -9 - -10 - -11 - -12 - -13 - -14 - -15 - -16 - -17 - -18 - -19 - -20, 208);
	assert(-1 - -2 - -3 - -4 - -5 - -6 - -7 - -8 - -9 - -10 - -11 - -12 - -13 - -14 - -15 - -16 - -17 - -18 - -19 - -20 - -21 - -22 - -23 - -24 - -25 - -26 - -27 - -28 - -29 - -30, 463);
}`
	),
	TestCase(
		'mul',
		`fn main() {
	assert(0 * 0, 0);
	assert(1 * 0, 0);
	assert(1 * 2, 2);
	assert(100 * 1, 100);
	assert(241241512.0 * 241241512.0 * 241241512.0, 1.4039644960680285e25);
	assert(1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10, 3628800);
	assert(1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 * 11 * 12 * 13 * 14 * 15 * 16 * 17 * 18 * 19 * 20, 2432902008176640000);
}`
	),
	TestCase(
		'mul with negatives',
		`fn main() {
	assert(0 * -1, 0);
	assert(-1 * 0, 0);
	assert(-1 * -1, 1);
	assert(1 * -1, -1);
	assert(-1 * 1, -1);
	assert(1 * -2, -2);
	assert(-2 * 1, -2);
	assert(1 * -3, -3);
	assert(-3 * 1, -3);
	assert(1 * -4 * -10, 40);
	assert(-1 * -2 * -3 * -4 * -5 * -6 * -7 * -8 * -9 * -10, 3628800);
	assert(-1 * -2 * -3 * -4 * -5 * -6 * -7 * -8 * -9 * -10 * -11 * -12 * -13 * -14 * -15 * -16, 20922789888000);
}`
	),
	TestCase(
		'div',
		`fn main() {
	assert(0 / 0, 0);
	assert(1 / 0, 0);
	assert(1 / 2, 0);
	assert(100 / 1, 100);
	assert(53252.0 / 1241205.0, 0.042903468806522696);
	assert(1 / 2 / 3 / 4 / 5 / 6 / 7 / 8 / 9 / 10, 0);
	assert(1 / 2 / 3 / 4 / 5 / 6 / 7 / 8 / 9 / 10 / 11 / 12 / 13 / 14 / 15 / 16 / 17 / 18 / 19 / 20, 0);
	assert(1 / 2 / 3 / 4 / 5 / 6 / 7 / 8 / 9 / 10 / 11 / 12 / 13 / 14 / 15 / 16 / 17 / 18 / 19 / 20 / 21 / 22 / 23 / 24 / 25 / 26 / 27 / 28 / 29 / 30, 0);
}`
	),
	TestCase(
		'div with negatives',
		`fn main() {
	assert(0 / -1, 0);
	assert(-1 / 0, 0);
	assert(-1 / -1, 1);
	assert(1 / -1, -1);
	assert(-1 / 1, -1);
	assert(1 / -2, -0);
	assert(-2 / 1, -2);
	assert(1 / -3, -0);
	assert(-3 / 1, -3);
	assert(1 / -4 / -10, 0);
	assert(-1 / -2 / -3 / -4 / -5 / -6 / -7 / -8 / -9 / -10, 0);
	assert(-1 / -2 / -3 / -4 / -5 / -6 / -7 / -8 / -9 / -10 / -11 / -12 / -13 / -14 / -15 / -16 / -17 / -18 / -19 / -20, 0);
	assert(-1 / -2 / -3 / -4 / -5 / -6 / -7 / -8 / -9 / -10 / -11 / -12 / -13 / -14 / -15 / -16 / -17 / -18 / -19 / -20 / -21 / -22 / -23 / -24 / -25 / -26 / -27 / -28 / -29 / -30, 0);
}`
	),
	TestCase(
		'pow',
		`fn main() {
	assert(0 ** 0, 1);
	assert(1 ** 0, 1);
	assert(1 ** 2, 1);
	assert(100 ** 1, 100);
	assert(100 ** 2, 10000);
	assert(53252.0 ** 12.0, 5.2003321105173854e56);
	assert(412 ** 4, 28813025536);
}
	`
	),
	TestCase(
		'expression',
		`fn main() {
	assert(436 + 3252 - 235 * 52 + 35 / 5 + 234 ** 3 * (1 - 2), -12821429);
}
	`
	),
	TestCase(
		'logic',
		`fn main() {
	assert(true && true, true);
	assert(true && false, false);
	assert(false && true, false);
	assert(false && false, false);
	assert(true || true, true);
	assert(true || false, true);
	assert(false || true, true);
	assert(false || false, false);
	assert(!true, false);
	assert(!false, true);
	assert(true == true, true);
	assert(true == false, false);
	assert(false == true, false);
	assert(false == false, true);
	assert(true != true, false);
	assert(true != false, true);
	assert(false != true, true);
	assert(false != false, false);
}`
	),
	TestCase(
		'swizzle',
		`fn main() {
	assert((1, 2).x, 1);
	assert((1, 2).y, 2);
	assert((1, 2).xy, (1, 2));
	assert((1, 2, 3).x, 1);
	assert((1, 2, 3).y, 2);
	assert((1, 2, 3).z, 3);
	assert((1, 2, 3).xy, (1, 2));
	assert((1, 2, 3).yz, (2, 3));
	assert((1, 2, 3).xyz, (1, 2, 3));
	assert((1, 2, 3, 4).x, 1);
	assert((1, 2, 3, 4).y, 2);
	assert((1, 2, 3, 4).z, 3);
	assert((1, 2, 3, 4).w, 4);
	
	assert((1, 2).yx, (2, 1));
	assert((1, 2, 3).yx, (2, 1));
	assert((1, 2, 3).yxz, (2, 1, 3));

	assert((1, 2, 3, 4).zxzx, (3, 1, 3, 1));
	assert((1, 2, 3, 4).rgb, (1, 2, 3));
}`
	),
	TestCase(
		'overloads',
		`fn main() {
	assert((1, 2, 3) * 2, (2, 4, 6));
	assert((1, 2, 3) * (1, 2, 3), (1, 4, 9));
}`
	),
	TestCase(
		'vector math',
		`fn main() {
	assert((1, 2, 3) + (1, 2, 3), (2, 4, 6));
	assert((1, 2, 3) - (1, 2, 3), (0, 0, 0));
	assert((1, 2, 3) * (1, 2, 3), (1, 4, 9));
	assert((1, 2, 3) / (1, 2, 3), (1, 1, 1));
	assert((1, 2, 3) ** (1, 2, 3), (1, 4, 27));
	assert((1, 2, 3) % (1, 2, 3), (0, 0, 0));
	
	assert((1, 2, 3) + 4, (5, 6, 7));
	assert((1, 2, 3) - 4, (-3, -2, -1));
	assert((1, 2, 3) * 4, (4, 8, 12));
	assert((1, 2, 3) / 4, (0, 0, 0));
	assert(float3(1, 2, 3) / 4.0, (0.25, 0.5, 0.75));
	assert((1, 2, 3) ** 4, (1, 16, 81));
	assert((1, 2, 3) % 2, (1, 0, 1));

}`
	)
];
