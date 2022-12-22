import { TestCase } from './test';

export default () => [
	TestCase(
		'array',
		`
fn main() {
	let a = [1, 2, 3];
	assert(1, a[0]);
	assert(2, a[1]);
	assert(3, a[2]);
	assert(3, a.len());
	a.push(4);
	assert(4, a[3]);
	a[0] = 5;
	assert(5, a[0]);
	a[0] = a[1] + a[2];
	assert(5, a[0]);
}`
	),
	TestCase(
		'struct',
		`
		struct Player {
			name: string,
			health: int
		}
		
		fn main() {
			let p = Player{
				name: "Player 1",
				health: 100.2,
			};
		
			let v = [p];
		
			assert(v[0].name, "Player 1");

			p.name = "Player 2";
		}`
	)
];
