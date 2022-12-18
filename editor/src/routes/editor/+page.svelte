<script lang="ts">
	import MonacoEditor from 'src/components/editor/MonacoEditor.svelte';
	import ShadeupEditor from 'src/components/editor/ShadeupEditor.svelte';
	import ShadeupRenderer from 'src/components/engine/ShadeupRenderer.svelte';
	import type ShadeupEnvironment from 'src/shadeup/environment';
	import { makeEnvironment } from 'src/shadeup/runner';
	import { onMount } from 'svelte';

	let canvasEl: HTMLCanvasElement | null = null;
	let scalingFactor = 1;

	let env: ShadeupEnvironment | null = null;

	let js = '';

	function handleSourceChange() {
		js = env?.generateFile('main.shadeup') || '';
	}

	onMount(async () => {
		env = await makeEnvironment(
			`
import invert from "test.shadeup";

let g = 0.01;

fn main() {
    g = g + 0.01;
    if (g > 1) { g = 0; }
	let y = 123;
	let x = shader {
		pixel = (invert(g), 1, 2, 3);
	};

	draw(x);
}`,
			[]
		);

		env.addEventListener('change', handleSourceChange);
	});
</script>

<main class="mx-auto">
	<div class="grid grid-cols-2">
		<div class="pl-4 pr-2">
			<div class="bg-black rounded-lg overflow-hidden text-white">
				{#if js}
					<MonacoEditor source={js} language="javascript" minimap={false} />
				{/if}
				<ShadeupRenderer environment={env} />
			</div>
		</div>
		<div class="pr-4 pl-2">
			<div class="bg-black">
				{#if env}
					<ShadeupEditor environment={env} filename="main.shadeup" minimap={true} />
				{/if}
			</div>
		</div>
	</div>
</main>
