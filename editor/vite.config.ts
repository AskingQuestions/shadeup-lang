import { defineConfig } from 'vite';

import wasm from 'vite-plugin-wasm';
// const prefix = `monaco-editor/esm/vs`;
import { sveltekit } from '@sveltejs/kit/vite';
import type { UserConfig } from 'vite';

const config: UserConfig = {
	plugins: [sveltekit(), wasm()],
	resolve: {
		alias: {
			src: '/src'
		}
	},
	worker: {
		format: 'es',
		plugins: [wasm()]
	}
	// build: {
	// 	rollupOptions: {
	// 		output: {
	// 			manualChunks: {
	// 				jsonWorker: [`${prefix}/language/json/json.worker`],
	// 				cssWorker: [`${prefix}/language/css/css.worker`],
	// 				htmlWorker: [`${prefix}/language/html/html.worker`],
	// 				tsWorker: [`${prefix}/language/typescript/ts.worker`],
	// 				editorWorker: [`${prefix}/editor/editor.worker`]
	// 			}
	// 		}
	// 	}
	// }
};

export default config;
