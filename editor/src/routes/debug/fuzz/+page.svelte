<script>
	import TestCase from 'src/components/fuzz/TestCase.svelte';
	import { tester, resetCases } from 'src/store/test';
	import { onMount } from 'svelte';

	onMount(() => {
		resetCases();
	});
</script>

<main class="max-w-screen-xl mx-auto">
	<secton>
		<h1 class="px-16 mb-2 text-4xl text-purple-600 font-title">Shadeup</h1>
		<h2 class="px-16 text-6xl text-gray-200 font-title font-bold">Test Suite</h2>
	</secton>

	<div class="mt-16">
		{#each [...$tester.cases.keys()] as group}
			<h2 class="px-16 mb-4 text-4xl font-bold text-gray-400 font-title capitalize">{group}</h2>

			<section class="space-y-4 px-8">
				{#each $tester.cases.get(group) || [] as _case}
					<TestCase instance={_case} />
				{/each}
			</section>
		{/each}
	</div>
</main>
