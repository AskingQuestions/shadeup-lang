import basic from './basic';
import type { TestCaseInstance } from './test';

export default new Map<string, TestCaseInstance[]>(
	Object.entries({
		basic: basic()
	})
);
