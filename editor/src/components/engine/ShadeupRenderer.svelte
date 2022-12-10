<script lang="ts">
	import Frame from 'src/shadeup/engine/frame.html?raw';
	import type ShadeupEnvironment from 'src/shadeup/environment';
	import { onMount, onDestroy } from 'svelte';

	export let environment: ShadeupEnvironment | null = null;

	let iframe: HTMLIFrameElement | null = null;

	function handleResize() {
		if (iframe) {
			let targetWidth = iframe.parentElement?.clientWidth || 1920;
			let targetHeight = targetWidth * (9 / 16);
			iframe.style.width = `${targetWidth}px`;
			iframe.style.height = `${targetHeight}px`;
		}
	}

	function sendFrameCode() {
		if (iframe && environment) {
			let js = environment.generateFile('main.shadeup');
			console.log(environment, JSON.stringify(js));
			iframe.contentWindow?.postMessage(
				{
					type: 'frame',
					source: js
				},
				'*'
			);
		}
	}

	function handleFileChange() {
		console.log('Got file change');
		sendFrameCode();
	}

	$: {
		if (environment) {
			sendFrameCode();
			environment.addEventListener('change', handleFileChange);
		}
	}

	onMount(() => {
		handleResize();
	});

	onDestroy(() => {
		if (environment) {
			environment.removeEventListener('change', handleFileChange);
		}
	});
</script>

<svelte:window on:resize={handleResize} />

<iframe
	style="border: 0;"
	title="shadeup renderer"
	sandbox="allow-scripts"
	srcdoc={Frame}
	bind:this={iframe}
/>
