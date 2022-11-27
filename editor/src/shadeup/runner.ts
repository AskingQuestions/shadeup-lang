import ShadeupEnvironment from './environment';
import frame from './frame.html?raw';
import type { ShadeupExternalSymbol } from './symbol';

export async function simpleRun(source: string, expose: ShadeupExternalSymbol[] = []) {
	let env = await makeEnvironment(source, expose);

	return await runEnvironment(env);
}

export async function makeEnvironment(source: string, expose: ShadeupExternalSymbol[] = []) {
	let env = new ShadeupEnvironment();

	await env.load();

	for (let sym of expose) {
		env.addSymbol(sym);
	}

	env.updateFile('main.shadeup', source);

	return env;
}

export async function runEnvironmentLong(env: ShadeupEnvironment, cb: (message: any) => void) {
	env.evaluate('main.shadeup');

	let js = env.generateFile('main.shadeup');

	let iframe = document.createElement('iframe');
	iframe.sandbox.add('allow-scripts');
	iframe.srcdoc = frame;
	iframe.style.display = 'none';

	let windowListener = (e: MessageEvent) => {
		var frame = document.getElementById('sandboxed');
		if (e.origin === 'null' && e.source === iframe.contentWindow) {
			cb(e.data);
		}
	};

	window.addEventListener('message', windowListener);

	iframe.onload = () => {
		if (iframe.contentWindow) {
			iframe.contentWindow.postMessage(js, '*');
		} else {
		}
	};
	document.body.appendChild(iframe);

	return () => {
		iframe.remove();

		window.removeEventListener('message', windowListener);
	};
}

export async function runEnvironment(env: ShadeupEnvironment) {
	env.evaluate('main.shadeup');

	let js = env.generateFile('main.shadeup');

	return await new Promise((resolve) => {
		let iframe = document.createElement('iframe');
		iframe.sandbox.add('allow-scripts');
		iframe.srcdoc = frame;
		iframe.style.display = 'none';

		let windowListener = (e: MessageEvent) => {
			var frame = document.getElementById('sandboxed');
			if (e.origin === 'null' && e.source === iframe.contentWindow) {
				resolve(e.data);
				window.removeEventListener('message', windowListener);
			}
		};

		window.addEventListener('message', windowListener);

		iframe.onload = () => {
			if (iframe.contentWindow) {
				iframe.contentWindow.postMessage(js, '*');
			} else {
				console.log('no content window');

				resolve(false);
			}
		};
		document.body.appendChild(iframe);
	});
}
