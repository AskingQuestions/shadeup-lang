<script>
	import { faPlus } from '@fortawesome/free-solid-svg-icons';
	import Fa from 'svelte-fa';
	import '../app.scss';
</script>

<header class="fixed w-full z-40">
	<nav class="bg-white border-gray-200 py-2.5 dark:bg-gray-900">
		<div class="flex flex-wrap items-center justify-between max-w-screen-xl px-4 mx-auto">
			<a href="#" class="flex items-center">
				<img src="/logo.svg" class="h-6 mr-3 sm:h-9" alt="Shadeup Logo" />
				<span class="self-center text-xl font-title whitespace-nowrap dark:text-white">Shadeup</span
				>
			</a>
			<div class="flex items-center lg:order-2">
				<div class="hidden mt-2 mr-4 sm:inline-block">
					<a
						class="github-button"
						href="https://github.com/themesberg/landwind"
						data-size="large"
						data-icon="octicon-star"
						data-show-count="true"
						aria-label="Star themesberg/landwind on GitHub">Star</a
					>
				</div>
				<!-- <a href="#" class="text-gray-800 dark:text-white hover:bg-gray-50 focus:ring-4 focus:ring-gray-300 font-medium rounded-lg text-sm px-4 lg:px-5 py-2 lg:py-2.5 sm:mr-2 dark:hover:bg-gray-700 focus:outline-none dark:focus:ring-gray-800">Log in</a> -->
				<a
					href="https://themesberg.com/product/tailwind-css/landing-page"
					class="text-white bg-purple-700 hover:bg-purple-800 focus:ring-4 focus:ring-purple-300 font-medium rounded-lg text-sm px-4 lg:px-5 py-2 lg:py-2.5 sm:mr-2 lg:mr-0 dark:bg-purple-600 dark:hover:bg-purple-700 focus:outline-none dark:focus:ring-purple-800"
					><Fa icon={faPlus} class="mr-2 inline-block" /> New Shadeup</a
				>
				<button
					data-collapse-toggle="mobile-menu-2"
					type="button"
					class="inline-flex items-center p-2 ml-1 text-sm text-gray-500 rounded-lg lg:hidden hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-gray-200 dark:text-gray-400 dark:hover:bg-gray-700 dark:focus:ring-gray-600"
					aria-controls="mobile-menu-2"
					aria-expanded="false"
				>
					<span class="sr-only">Open main menu</span>
					<svg
						class="w-6 h-6"
						fill="currentColor"
						viewBox="0 0 20 20"
						xmlns="http://www.w3.org/2000/svg"
						><path
							fill-rule="evenodd"
							d="M3 5a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 10a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1zM3 15a1 1 0 011-1h12a1 1 0 110 2H4a1 1 0 01-1-1z"
							clip-rule="evenodd"
						/></svg
					>
					<svg
						class="hidden w-6 h-6"
						fill="currentColor"
						viewBox="0 0 20 20"
						xmlns="http://www.w3.org/2000/svg"
						><path
							fill-rule="evenodd"
							d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
							clip-rule="evenodd"
						/></svg
					>
				</button>
			</div>
			<div
				class="items-center justify-between hidden w-full lg:flex lg:w-auto lg:order-1"
				id="mobile-menu-2"
			>
				<ul class="flex flex-col mt-4 font-medium lg:flex-row lg:space-x-8 lg:mt-0">
					<li>
						<a
							href="/"
							class="block py-2 pl-3 pr-4 text-white bg-purple-700 rounded lg:bg-transparent lg:text-purple-700 lg:p-0 dark:text-white"
							aria-current="page">Home</a
						>
					</li>
					<li>
						<a
							href="/debug/fuzz"
							class="block py-2 pl-3 pr-4 text-gray-700 border-b border-gray-100 hover:bg-gray-50 lg:hover:bg-transparent lg:border-0 lg:hover:text-purple-700 lg:p-0 dark:text-gray-400 lg:dark:hover:text-white dark:hover:bg-gray-700 dark:hover:text-white lg:dark:hover:bg-transparent dark:border-gray-700"
							>Fuzz</a
						>
					</li>
					<li>
						<a
							href="/browse"
							class="block py-2 pl-3 pr-4 text-gray-700 border-b border-gray-100 hover:bg-gray-50 lg:hover:bg-transparent lg:border-0 lg:hover:text-purple-700 lg:p-0 dark:text-gray-400 lg:dark:hover:text-white dark:hover:bg-gray-700 dark:hover:text-white lg:dark:hover:bg-transparent dark:border-gray-700"
							>Browse</a
						>
					</li>
					<li>
						<a
							href="/docs"
							class="block py-2 pl-3 pr-4 text-gray-700 border-b border-gray-100 hover:bg-gray-50 lg:hover:bg-transparent lg:border-0 lg:hover:text-purple-700 lg:p-0 dark:text-gray-400 lg:dark:hover:text-white dark:hover:bg-gray-700 dark:hover:text-white lg:dark:hover:bg-transparent dark:border-gray-700"
							>Docs</a
						>
					</li>
				</ul>
			</div>
		</div>
	</nav>
</header>
<div class="pt-24 pb-24 bg-gray-900">
	<slot />
</div>
<footer class="bg-white dark:bg-gray-800">
	<div class="max-w-screen-xl p-4 py-6 mx-auto lg:py-16 md:p-8 lg:p-10">
		<div class="grid grid-cols-2 gap-8 md:grid-cols-3 lg:grid-cols-5">
			<div>
				<h3 class="mb-6 text-sm font-semibold text-gray-900 uppercase dark:text-white">
					Organization
				</h3>
				<ul class="text-gray-500 dark:text-gray-400">
					<li class="mb-4">
						<a href="#" class=" hover:underline">About</a>
					</li>
					<li class="mb-4">
						<a href="#" class="hover:underline">Open source</a>
					</li>
					<li class="mb-4">
						<a href="#" class="hover:underline">Graphics</a>
					</li>
					<li class="mb-4">
						<a href="#" class="hover:underline">Blog</a>
					</li>
				</ul>
			</div>
			<div>
				<h3 class="mb-6 text-sm font-semibold text-gray-900 uppercase dark:text-white">
					Help center
				</h3>
				<ul class="text-gray-500 dark:text-gray-400">
					<li class="mb-4">
						<a href="#" class="hover:underline">Discord Server</a>
					</li>
					<li class="mb-4">
						<a href="#" class="hover:underline">Twitter</a>
					</li>
					<li class="mb-4">
						<a href="#" class="hover:underline">FAQ </a>
					</li>
					<li class="mb-4">
						<a href="#" class="hover:underline">Contact Us</a>
					</li>
				</ul>
			</div>
			<div>
				<h3 class="mb-6 text-sm font-semibold text-gray-900 uppercase dark:text-white">Legal</h3>
				<ul class="text-gray-500 dark:text-gray-400">
					<li class="mb-4">
						<a href="#" class="hover:underline">Privacy Policy</a>
					</li>
					<li class="mb-4">
						<a href="#" class="hover:underline">Licensing</a>
					</li>
					<li class="mb-4">
						<a href="#" class="hover:underline">Terms</a>
					</li>
				</ul>
			</div>

			<div>
				<h3 class="mb-6 text-sm font-semibold text-gray-900 uppercase dark:text-white">Docs</h3>
				<ul class="text-gray-500 dark:text-gray-400">
					<li class="mb-4">
						<a href="#" class="hover:underline">Library</a>
					</li>
					<li class="mb-4">
						<a href="#" class="hover:underline">Website</a>
					</li>
					<li class="mb-4">
						<a href="#" class="hover:underline">Editor</a>
					</li>
					<li class="mb-4">
						<a href="#" class="hover:underline">Language</a>
					</li>
				</ul>
			</div>
		</div>
		<hr class="my-6 border-gray-200 sm:mx-auto dark:border-gray-700 lg:my-8" />
		<div class="text-center">
			<a
				href="#"
				class="flex font-title items-center justify-center mb-5 text-2xl text-gray-900 dark:text-white"
			>
				<img src="/logo.svg" class="h-6 mr-3 sm:h-9" alt="Shadeup Logo" />
				Shadeup
			</a>
			<span class="block text-sm text-center text-gray-500 dark:text-gray-400"
				>© 2022-{new Date().getFullYear()} Shadeup. All Rights Reserved. Built with
			</span>
			<ul class="flex justify-center mt-5 space-x-5">
				<li>
					<a
						href="#"
						class="text-gray-500 hover:text-gray-900 dark:hover:text-white dark:text-gray-400"
					>
						<svg class="w-5 h-5" fill="currentColor" viewBox="0 0 24 24" aria-hidden="true"
							><path
								d="M8.29 20.251c7.547 0 11.675-6.253 11.675-11.675 0-.178 0-.355-.012-.53A8.348 8.348 0 0022 5.92a8.19 8.19 0 01-2.357.646 4.118 4.118 0 001.804-2.27 8.224 8.224 0 01-2.605.996 4.107 4.107 0 00-6.993 3.743 11.65 11.65 0 01-8.457-4.287 4.106 4.106 0 001.27 5.477A4.072 4.072 0 012.8 9.713v.052a4.105 4.105 0 003.292 4.022 4.095 4.095 0 01-1.853.07 4.108 4.108 0 003.834 2.85A8.233 8.233 0 012 18.407a11.616 11.616 0 006.29 1.84"
							/></svg
						>
					</a>
				</li>
				<li>
					<a
						href="#"
						class="text-gray-500 hover:text-gray-900 dark:hover:text-white dark:text-gray-400"
					>
						<svg class="w-5 h-5" fill="currentColor" viewBox="0 0 24 24" aria-hidden="true"
							><path
								fill-rule="evenodd"
								d="M12 2C6.477 2 2 6.484 2 12.017c0 4.425 2.865 8.18 6.839 9.504.5.092.682-.217.682-.483 0-.237-.008-.868-.013-1.703-2.782.605-3.369-1.343-3.369-1.343-.454-1.158-1.11-1.466-1.11-1.466-.908-.62.069-.608.069-.608 1.003.07 1.531 1.032 1.531 1.032.892 1.53 2.341 1.088 2.91.832.092-.647.35-1.088.636-1.338-2.22-.253-4.555-1.113-4.555-4.951 0-1.093.39-1.988 1.029-2.688-.103-.253-.446-1.272.098-2.65 0 0 .84-.27 2.75 1.026A9.564 9.564 0 0112 6.844c.85.004 1.705.115 2.504.337 1.909-1.296 2.747-1.027 2.747-1.027.546 1.379.202 2.398.1 2.651.64.7 1.028 1.595 1.028 2.688 0 3.848-2.339 4.695-4.566 4.943.359.309.678.92.678 1.855 0 1.338-.012 2.419-.012 2.747 0 .268.18.58.688.482A10.019 10.019 0 0022 12.017C22 6.484 17.522 2 12 2z"
								clip-rule="evenodd"
							/></svg
						>
					</a>
				</li>
			</ul>
		</div>
	</div>
</footer>
