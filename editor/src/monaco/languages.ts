import MonarchDef from './monarch.js';
import monaco from './monaco';

import 'monaco-editor/esm/vs/basic-languages/css/css.contribution';
import 'monaco-editor/esm/vs/basic-languages/xml/xml.contribution';
import 'monaco-editor/esm/vs/basic-languages/javascript/javascript.contribution';

import EditorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker';
import TsWorker from 'monaco-editor/esm/vs/language/typescript/ts.worker?worker';
import JsonWorker from 'monaco-editor/esm/vs/language/json/json.worker?worker';
import CssWorker from 'monaco-editor/esm/vs/language/css/css.worker?worker';
import HtmlWorker from 'monaco-editor/esm/vs/language/html/html.worker?worker';

export default function registerLanguages() {
	// eslint-disable-next-line @typescript-eslint/ban-ts-comment
	// @ts-ignore
	window.MonacoEnvironment = {
		getWorker(_: string, label: string) {
			if (label === 'typescript' || label === 'javascript') return new TsWorker();
			if (label === 'json') return new JsonWorker();
			if (label === 'css') return new CssWorker();
			if (label === 'html') return new HtmlWorker();
			return new EditorWorker();
		}
	};

	monaco.languages.register({
		id: 'plaintext',
		extensions: ['.txt'],
		aliases: ['PLAINTEXT', 'plaintext'],
		mimetypes: ['text/plain']
	});

	monaco.languages.register({
		id: 'shadeup',
		extensions: ['.shadeup'],
		aliases: ['SHADEUP', 'shadeup'],
		mimetypes: ['text/shadeup']
	});

	monaco.languages.setMonarchTokensProvider('shadeup', MonarchDef);

	monaco.languages.registerCompletionItemProvider('shadeup', {
		triggerCharacters: ['.'],
		provideCompletionItems(model, position) {
			return model.$shd.provideCompletionItems(model, position);
		}
	});

	monaco.editor.registerCommand('editor.action.shadeup.import', (a, ed, e) => {
		return ed.$shd.provideImport(e);
	});

	monaco.languages.registerHoverProvider('shadeup', {
		provideHover(model, pos) {
			return model.$shd.provideHover(model, pos);
		}
	});

	monaco.languages.registerDefinitionProvider('shadeup', {
		provideDefinition(model, pos) {
			return model.$shd.provideDefinition(model, pos);
		}
	});
}
