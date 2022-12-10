<script lang="ts">
	import { faSpinner } from '@fortawesome/free-solid-svg-icons';

	import type ShadeupEnvironment from 'src/shadeup/environment.js';
	import { onMount, createEventDispatcher } from 'svelte';
	import Fa from 'svelte-fa';

	const dispatch = createEventDispatcher();

	let el: HTMLElement | null = null;

	export let filename: string = '';
	export let environment: ShadeupEnvironment;

	export let minimap: boolean = true;

	let gid = 0;

	let loaded = false;

	onMount(async () => {
		gid = ((window as any).$shd_monaco || 0) + 1;
		(window as any).$shd_monaco = gid;

		const registerLanguages = (await import('src/monaco/languages.js')).default;
		if (gid === 1) {
			registerLanguages();
		}
		const connectEditorToEnvironment = (await import('src/monaco/shadeup.js')).default;

		const MonacoEditorInstance = (await import('../../monaco/editor.js')).MonacoEditorInstance;
		let monacoFilename = `${gid}_${filename}`;
		const editor = new MonacoEditorInstance(monacoFilename, {
			minimap: {
				enabled: minimap
			}
		});

		loaded = true;
		setTimeout(() => {
			if (el) editor.mount(el);
			editor.editor?.setValue(environment.files.get(filename) || '');
			editor.editor?.onDidChangeModelContent(() => {
				dispatch('change', { value: editor.editor?.getValue() || '' });
			});

			connectEditorToEnvironment(filename, monacoFilename, editor, environment);
		}, 1);
	});
</script>

<div class="">
	{#if loaded}
		<div class="h-80 monaco-editor" data-gid={gid} bind:this={el} />
	{:else}
		<div class="h-40 bg-gray-900 flex justify-center items-center flex-col text-gray-500">
			<Fa icon={faSpinner} class="animate-spin text-2xl h-8 w-8 sm:h-10 sm:w-10" />
			<span>Loading editor</span>
		</div>
	{/if}
</div>
