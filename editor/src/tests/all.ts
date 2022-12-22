import basic from './basic';
import flow from './flow';
import functions from './functions';
import structures from './structures';
import type { TestCaseInstance } from './test';

export default () =>
	new Map<string, TestCaseInstance[]>(
		Object.entries({
			// basic: basic(),
			// flow: flow(),
			// functions: functions(),
			structures: structures()
		})
	);
