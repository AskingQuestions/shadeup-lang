export class TestCaseInstance {
	name: string;
	source: string;
	tester: any;
	started: Date | null = null;
	stopped: Date | null = null;

	running = true;
	passed = false;

	asserts: { print: string; passed: boolean }[] = [];

	log: string[] = [];

	constructor(name: string, source: string, tester: any) {
		this.name = name;
		this.source = source;
		this.tester = tester;
	}

	start() {
		this.running = true;
		this.started = new Date();
	}

	stop() {
		this.running = false;
		console.log('Stopping test case', this.name);
		this.stopped = new Date();

		this.passed = true;
		for (const assert of this.asserts) {
			if (!assert.passed) {
				this.passed = false;
				break;
			}
		}
	}

	getDuration(): string {
		if (!this.started) return 'not started';
		let end = this.stopped || new Date();
		const duration = end.getTime() - this.started.getTime();
		return `${(duration / 1000).toFixed(1)}s`;
	}

	getAssertsCount(): string {
		return `${this.asserts.filter((a) => a.passed).length}/${this.asserts.length}`;
	}

	getStatus(): string {
		if (this.running) {
			return 'running';
		}
		if (this.passed) {
			return 'passed';
		}
		return 'failed';
	}
}

export function TestCase(name: string, source: string, tester = null) {
	return new TestCaseInstance(name, source, tester);
}
