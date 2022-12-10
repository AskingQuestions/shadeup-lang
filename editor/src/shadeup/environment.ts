// parse("hello world");

import type { ShadeupExternalSymbol } from './symbol.js';

type ShadeupAlert = {
	level: 'info' | 'warning' | 'error';
	message: string;
	location: Number;
};

let wasm_cache: any = null;

export function loadWasm() {
	if (wasm_cache) {
		return wasm_cache;
	}

	let exp = import('../../../parser-wasm/pkg/parser_wasm_bg.js');
	wasm_cache = exp;

	return exp;
}

export default class ShadeupEnvironment extends EventTarget {
	files: Map<string, string>;
	alerts: Map<string, ShadeupAlert[]>;
	env: any;
	mod: any;

	constructor() {
		super();
		this.files = new Map();
		this.alerts = new Map();
	}

	async load() {
		let exp = await loadWasm();
		this.mod = exp;
		console.log(exp);
		this.env = exp.make_environment();

		this.updateFile(
			'geometry',
			'fn Cube() {} fn Pyramid() {} fn Cylinder() {} fn Sphere() {} fn IcoSphere() {} fn Plane() {}'
		);
		this.evaluate('geometry');
	}

	loaded() {
		if (this.env) {
			return Promise.resolve();
		}
		return new Promise((resolve) => {
			if (this.env) {
				resolve();
			} else {
				const interval = setInterval(() => {
					if (this.env) {
						clearInterval(interval);
						resolve();
					}
				}, 100);
			}
		});
	}

	addSymbol(sym: ShadeupExternalSymbol) {
		if (sym.kind == 'function') {
			let send = {
				name: sym.name,
				parameters: sym.parameters,
				return_type: sym.outType,
				js: sym.js
			};
			this.mod.add_native_function(this.env, send);
		}
	}

	getAlerts(filename: string) {
		return this.alerts.get(filename) || [];
	}

	getIntellisense(filename: string) {
		return this.mod.get_intellisense(this.env, filename) || [];
	}

	evaluate(filename: string): boolean {
		let now = performance.now();
		const content = this.files.get(filename);
		// const ast = this.parse(content);

		// console.log(
		//   ast.alerts.length + " alerts in " + (performance.now() - now) + "ms"
		// );

		// this.alerts.set(filename, ast.alerts);

		let successfulParse = this.mod.parse_file(this.env, filename);

		let alerts = this.mod.get_file_alerts(this.env, filename);
		this.alerts.set(filename, alerts);

		return successfulParse;
	}

	getAst(filename: string) {
		return this.mod.get_ast(this.env, filename);
	}

	updateFile(filename: string, content: string) {
		this.files.set(filename, content);
		this.mod.set_file(this.env, filename, content);
		setTimeout(() => {
			this.dispatchEvent(new CustomEvent('change', { detail: filename }));
		}, 0);
	}

	generateFile(filename: string): string {
		return this.mod.generate_file(this.env, filename);
	}

	getImports(filename: string) {
		return this.mod.get_imports(this.env, filename);
	}

	getSymbols() {
		return this.mod.get_symbols(this.env);
	}
}
