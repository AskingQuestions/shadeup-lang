import type ShadeupEnvironment from '../shadeup/environment.js';
import type { MonacoEditorInstance } from './editor.js';
import { default as AnsiUp } from 'ansi_up';
import monaco from './monaco';

export default function connectEditorToEnvironment(
	filename: string,
	monacoFilename: string,
	editor: MonacoEditorInstance,
	env: ShadeupEnvironment
) {
	let ed = editor.editor;

	function updateAlerts() {
		let alerts = [...env.files.keys()].map((k) => env.getAlerts(k)).flat();
		monaco.editor.setModelMarkers(ed.getModel(), 'shadeup', [
			...alerts.map((e) => {
				const ansi_up = new AnsiUp();

				let html = ansi_up.ansi_to_html(e.message);
				return {
					severity: monaco.MarkerSeverity.Error,
					startLineNumber: e.location.start_line_and_column[0],
					startColumn: e.location.start_line_and_column[1] + 1,
					endLineNumber: e.location.end_line_and_column[0],
					endColumn: e.location.end_line_and_column[1] + 1,
					message: e.simple_message
				};
			})
		]);

		let consol = document.getElementById('console');
		if (consol) {
			consol.innerHTML = `${alerts
				.map((e) => {
					const ansi_up = new AnsiUp();

					let html = ansi_up.ansi_to_html(e.message);
					return `<pre>${html}</pre>`;
				})
				.join('')}`;
		}
	}

	function spanToRange(span: { start: number; end: number }) {
		let start = ed.getModel()!.getPositionAt(span.start);
		let end = ed.getModel()!.getPositionAt(span.end);
		return new monaco.Range(start.lineNumber, start.column, end.lineNumber, end.column);
	}

	function offsetToRange(offset: number) {
		let start = ed.getModel()!.getPositionAt(offset);
		let end = ed.getModel()!.getPositionAt(offset);
		return new monaco.Range(start.lineNumber, start.column, end.lineNumber, end.column);
	}

	function positionToOffset(position: monaco.IPosition) {
		return ed.getModel()!.getOffsetAt(position);
	}

	(ed.getModel() as any).$shd = {
		provideCompletionItems(model, position) {
			let suggestions: monaco.languages.CompletionItem[] = [];
			let offset = positionToOffset(position);

			let intel = env.getIntellisense(filename);

			for (let hint of intel) {
				if (hint.start <= offset && offset <= hint.end) {
					for (let suggestion of hint.suggestions) {
						if (suggestion.label.startsWith('__')) {
							continue;
						}
						let item: monaco.languages.CompletionItem = {
							label: suggestion.label,
							detail: suggestion.detail,
							insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
							kind:
								{
									Field: monaco.languages.CompletionItemKind.Field,
									Function: monaco.languages.CompletionItemKind.Function,
									Variable: monaco.languages.CompletionItemKind.Variable,
									Type: monaco.languages.CompletionItemKind.Struct
								}[suggestion.kind as string] || monaco.languages.CompletionItemKind.Function,
							documentation: suggestion.documentation,
							insertText: suggestion.value,
							range: spanToRange({ start: hint.start, end: hint.end })
						};
						suggestions.push(item);
					}
				}
			}

			let symbols = env.getSymbols();

			if (suggestions.length == 0) {
				suggestions = [
					...symbols
						.filter((e) => !e.imported || e.aliased)
						.map((e) => ({
							label: e.name,
							detail: e.file,
							insertText: e.name + (e.kind == 'function' ? '()' : ''),
							insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
							kind:
								e.kind == 'function'
									? monaco.languages.CompletionItemKind.Function
									: monaco.languages.CompletionItemKind.Struct,
							command: {
								id: 'editor.action.shadeup.import',
								title: 'Import',
								arguments: [ed, e]
							}
						})),
					{
						label: 'import',
						insertText: 'import ${1:} from "${2:}";',
						insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet
					}
				];
			}

			return {
				suggestions
			};
		},
		provideImport(a, e) {
			let sym = e.name;
			let path = e.file;
			// Parse current imports
			let mod = ed.getModel();
			let finalImport = 0;
			let imports = env.getImports(filename);

			if (e.imported || e.file == filename) {
				return;
			}

			for (let imp of imports) {
				if (imp.path == path) {
					if (imp.imports.find((i) => (i.alias || i.name) == e.name)) {
						return;
					}
					mod.pushEditOperations(
						[],
						[
							{
								range: offsetToRange(imp.imports[imp.imports.length - 1].span.start),
								text: sym + ', '
							}
						],
						() => null
					);
					return;
				}
				finalImport = imp.span.end;
			}

			let r = spanToRange({
				start: finalImport,
				end: finalImport
			});

			mod.pushEditOperations(
				[],
				[
					{
						range: {
							startLineNumber: r.startLineNumber - 1,
							startColumn: 1,
							endLineNumber: r.endLineNumber - 1,
							endColumn: 1
						},
						text: `import ${sym} from "${path}";\n`
					}
				],
				() => null
			);
		},
		provideHover(model, pos) {
			let hovers = [];
			let offset = positionToOffset(pos);

			let intel = env.getIntellisense(filename);

			for (let hint of intel) {
				if (hint.start <= offset && offset <= hint.end) {
					let item = {
						contents: [{ value: '```shadeup\n' + hint.label + '\n```' }],
						range: spanToRange({ start: hint.start, end: hint.end }),
						span: { start: hint.start, end: hint.end }
					};
					hovers.push(item);
				}
			}
			hovers.sort((a, b) => a.span.end - a.span.start - (b.span.end - b.span.start));

			if (hovers.length > 0) {
				return hovers[0];
			}
		},
		provideDefinition(model, pos) {
			let gotos = [];
			let offset = positionToOffset(pos);

			let intel = env.getIntellisense(filename);

			for (let hint of intel) {
				if (hint.start <= offset && offset <= hint.end) {
					let item = {
						range: spanToRange({
							start: hint.goto_offset,
							end: hint.goto_offset
						}),
						span: { start: hint.start, end: hint.end }
					};
					gotos.push(item);
				}
			}
			gotos.sort((a, b) => a.span.end - a.span.start - (b.span.end - b.span.start));

			if (gotos.length > 0) {
				return {
					uri: monaco.Uri.parse(monacoFilename),
					range: gotos[0].range
				};
			}
		}
	};

	function update() {
		let now = performance.now();
		env.updateFile(filename, ed.getValue() + '\n');
		let successfulParse = env.evaluate(filename);

		updateAlerts();

		console.log(`took ${performance.now() - now}ms`);
	}

	update();

	ed.onDidChangeModelContent((e) => {
		update();
	});
}
