const __SHADERS = [

];

/* cpu_only async */async function primitives_sleep(value) {
await __native_sleep(value)
}

/* cpu_only async */async function test__shadeup_main() {
(await primitives_sleep(__int___cast_from_scalar((1 & 0xffffffff))));

}

function __int___cast_from_scalar(other) {
return other & 0xffffffff;
}


Parse: 18.2953ms
Process: 359.6┬╡s
Generate: 46.2492ms
Total: 65.1054ms
