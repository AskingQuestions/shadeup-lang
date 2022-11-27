import monaco from './monaco';

export class MonacoEditorInstance {
	element: HTMLElement | null = null;
	editor: monaco.editor.IStandaloneCodeEditor | null = null;
	filename: string = '';
	options: monaco.editor.IStandaloneEditorConstructionOptions = {};

	$_resize: () => void = () => {};

	constructor(filename: string, options: monaco.editor.IStandaloneEditorConstructionOptions = {}) {
		this.filename = filename;
		this.options = options;
	}

	resize() {
		if (this.editor) {
			this.editor.layout();
		}
	}

	mount(element: HTMLElement) {
		this.editor = monaco.editor.create(element, {
			model: monaco.editor.createModel('', 'shadeup', monaco.Uri.parse(this.filename)),
			theme: 'vs-dark',
			...this.options
		});

		this.$_resize = () => this.resize();
		window.addEventListener('resize', this.$_resize);
	}

	unmount() {
		if (!this.editor) return;
		this.editor.dispose();
		window.removeEventListener('resize', this.$_resize);
	}

	update(newCode: string) {
		if (!this.editor) return;
		this.editor.setValue(newCode);
	}
}
