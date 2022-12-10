<script lang="ts">
	import { faSpinner } from '@fortawesome/free-solid-svg-icons';
	// import { editor } from 'monaco-editor';

	import type ShadeupEnvironment from 'src/shadeup/environment.js';
	import { onMount } from 'svelte';
	import Fa from 'svelte-fa';
	import type { MonacoEditorInstance } from '../../monaco/editor.js';

	let el: HTMLElement | null = null;

	export let source = '';
	export let language = 'javascript';

	export let minimap: boolean = true;

	let gid = 0;

	let loaded = false;

	let editorInst: MonacoEditorInstance | null = null;

	$: {
		if (editorInst) editorInst.editor?.setValue(source);
	}

	onMount(async () => {
		gid = ((window as any).$lang_monaco || 0) + 1;
		(window as any).$lang_monaco = gid;

		const MonacoEditorInstance = (await import('../../monaco/editor.js')).MonacoEditorInstance;
		editorInst = new MonacoEditorInstance(`lang_${gid}`, {
			language,
			minimap: {
				enabled: minimap
			}
		});
		loaded = true;
		setTimeout(() => {
			if (el) editorInst?.mount(el);
			editorInst?.editor?.setValue(source);
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
