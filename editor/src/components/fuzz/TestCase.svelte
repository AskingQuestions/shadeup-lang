<script lang="ts">
	import {
		faArrowDown,
		faCaretDown,
		faCheckCircle,
		faChevronDown,
		faClose,
		faSpinner
	} from '@fortawesome/free-solid-svg-icons';
	import type { TestCaseInstance } from 'src/tests/test';

	import { default as AnsiUp } from 'ansi_up';
	import { slide } from 'svelte/transition';

	import {
		makeEnvironment,
		runEnvironment,
		runEnvironmentLong,
		simpleRun
	} from 'src/shadeup/runner';

	import Fa from 'svelte-fa';
	import { onMount } from 'svelte';
	import { ShadeupExternalSymbol } from 'src/shadeup/symbol';
	import type ShadeupEnvironment from 'src/shadeup/environment';
	import MonacoEditor from '../editor/MonacoEditor.svelte';

	export let instance: TestCaseInstance;

	let open = false;

	$: icon = {
		running: faSpinner,
		passed: faCheckCircle,
		failed: faClose
	}[instance.getStatus()];

	$: color = {
		running: 'gray-500',
		passed: 'green-500',
		failed: 'red-500'
	}[instance.getStatus()];

	$: text = {
		running: 'This case is running...',
		passed: 'This case succeeded!',
		failed: 'This case failed!'
	}[instance.getStatus()];

	let duration = '';

	duration = instance.getDuration();

	let interval = setInterval(() => {
		duration = instance.getDuration();
		if (!instance.running) {
			clearInterval(interval);
		}
	}, 100);

	let syms = [
		ShadeupExternalSymbol.makeFunction(
			'assert',
			'void',
			[
				['a', 'any', false],
				['b', 'any', false]
			],
			`
			console.log("assert", a,b);
			postMessage({print: \`\${a} == \${b}\`, passed: a.toString() == b.toString()});
			`
		)
	];

	let environment: ShadeupEnvironment | null = null;

	onMount(async () => {
		environment = await makeEnvironment(instance.source, syms);
		instance.start();
		let killTimer: any;

		function finalize() {
			if (environment?.alerts) {
				for (let f of environment.alerts) {
					if (f[0] == 'main.shadeup') {
						for (let alert of f[1]) {
							const ansi_up = new AnsiUp();

							let html = ansi_up.ansi_to_html(alert.message);
							instance.asserts.push({
								print: html,
								passed: false
							});
							instance.passed = false;
						}
					}
				}
			}
		}

		let kill = await runEnvironmentLong(environment, (message: any) => {
			if (message && message.print) {
				instance.asserts.push(message);
			} else {
				instance.stop();
				finalize();
				instance = instance;
				kill();
				clearTimeout(killTimer);
			}
		});

		killTimer = setTimeout(() => {
			instance.stop();
			finalize();
			instance = instance;
			kill();
		}, 10000);
	});
</script>

<div>
	<div
		class="panel cursor-pointer"
		on:click={() => (open = !open)}
		on:keypress={() => (open = !open)}
	>
		<span
			class="absolute right-4 top-4 rounded-full bg-{color} px-3 py-1.5 text-xs font-medium text-black"
		>
			{duration}
		</span>

		<div class="mt-4 text-{color} sm:pr-8">
			<Fa
				{icon}
				class="text-2xl h-8 w-8 sm:h-10 sm:w-10 {instance.running ? 'animate-spin' : ''}"
			/>

			<h3 class="mt-4 text-xl font-bold text-gray-400">
				{instance.name}
				{instance.getAssertsCount()}
			</h3>

			<p class="mt-2 hidden text-sm sm:block">{text}</p>
		</div>
		<button
			class="absolute bottom-4 right-4 p-4 rounded-full hover:bg-gray-600"
			class:rotate-180={open}><Fa icon={faChevronDown} class="text-white" /></button
		>
		{#if instance.getStatus() == 'failed'}
			<div class="space-y-4 my-10">
				{#each instance.asserts as assert}
					<div
						class=" rounded-sm p-4 text-white whitespace-pre overflow-auto font-mono border-solid border border-{assert.passed
							? 'green'
							: 'red'}-500"
					>
						{@html assert.print}
					</div>
				{/each}
			</div>
		{/if}
	</div>

	{#if open}
		<div class="panel mt-4" transition:slide>
			{#if environment}
				<MonacoEditor {environment} filename="main.shadeup" minimap={false} />
			{:else}
				<div class="h-20 bg-gray-900 flex justify-center items-center flex-col">
					<Fa {icon} class="animate-spin text-2xl h-8 w-8 sm:h-10 sm:w-10" />
					<span>Loading environment</span>
				</div>
			{/if}
		</div>
	{/if}
</div>
