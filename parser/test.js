const __SHADERS = [

];

function __swiz_cross_int4_2(a) {
return a[2];
}

function __swiz_cross_int2_1(a) {
return a[1];
}

function __swiz_cross_int3_2(a) {
return a[2];
}

function __swiz_cross_int4_0(a) {
return a[0];
}

function test__shadeup_assert(a, b) {

}

function __swiz_cross_int3_0(a) {
return a[0];
}

function test__shadeup_main() {
test__shadeup_assert(__swiz_cross_int2_0(__int2___make_vec((1 & 0xffffffff), (2 & 0xffffffff))), (1 & 0xffffffff));
test__shadeup_assert(__swiz_cross_int2_1(__int2___make_vec((1 & 0xffffffff), (2 & 0xffffffff))), (2 & 0xffffffff));
test__shadeup_assert(__swiz_cross_int2_0_1(__int2___make_vec((1 & 0xffffffff), (2 & 0xffffffff))), __int2___make_vec((1 & 0xffffffff), (2 & 0xffffffff)));
test__shadeup_assert(__swiz_cross_int3_0(__int3___make_vec((1 & 0xffffffff), (2 & 0xffffffff), (3 & 0xffffffff))), (1 & 0xffffffff));
test__shadeup_assert(__swiz_cross_int3_1(__int3___make_vec((1 & 0xffffffff), (2 & 0xffffffff), (3 & 0xffffffff))), (2 & 0xffffffff));
test__shadeup_assert(__swiz_cross_int3_2(__int3___make_vec((1 & 0xffffffff), (2 & 0xffffffff), (3 & 0xffffffff))), (3 & 0xffffffff));
test__shadeup_assert(__swiz_cross_int3_0_1(__int3___make_vec((1 & 0xffffffff), (2 & 0xffffffff), (3 & 0xffffffff))), __int2___make_vec((1 & 0xffffffff), (2 & 0xffffffff)));
test__shadeup_assert(__swiz_cross_int3_1_2(__int3___make_vec((1 & 0xffffffff), (2 & 0xffffffff), (3 & 0xffffffff))), __int2___make_vec((2 & 0xffffffff), (3 & 0xffffffff)));
test__shadeup_assert(__swiz_cross_int3_0_1_2(__int3___make_vec((1 & 0xffffffff), (2 & 0xffffffff), (3 & 0xffffffff))), __int3___make_vec((1 & 0xffffffff), (2 & 0xffffffff), (3 & 0xffffffff)));
test__shadeup_assert(__swiz_cross_int4_0(__int4___make_vec((1 & 0xffffffff), (2 & 0xffffffff), (3 & 0xffffffff), (4 & 0xffffffff))), (1 & 0xffffffff));
test__shadeup_assert(__swiz_cross_int4_1(__int4___make_vec((1 & 0xffffffff), (2 & 0xffffffff), (3 & 0xffffffff), (4 & 0xffffffff))), (2 & 0xffffffff));
test__shadeup_assert(__swiz_cross_int4_2(__int4___make_vec((1 & 0xffffffff), (2 & 0xffffffff), (3 & 0xffffffff), (4 & 0xffffffff))), (3 & 0xffffffff));
test__shadeup_assert(__swiz_cross_int4_3(__int4___make_vec((1 & 0xffffffff), (2 & 0xffffffff), (3 & 0xffffffff), (4 & 0xffffffff))), (4 & 0xffffffff));
test__shadeup_assert(__swiz_cross_int2_1_0(__int2___make_vec((1 & 0xffffffff), (2 & 0xffffffff))), __int2___make_vec((2 & 0xffffffff), (1 & 0xffffffff)));
test__shadeup_assert(__swiz_cross_int3_1_0(__int3___make_vec((1 & 0xffffffff), (2 & 0xffffffff), (3 & 0xffffffff))), __int2___make_vec((2 & 0xffffffff), (1 & 0xffffffff)));
test__shadeup_assert(__swiz_cross_int3_1_0_2(__int3___make_vec((1 & 0xffffffff), (2 & 0xffffffff), (3 & 0xffffffff))), __int3___make_vec((2 & 0xffffffff), (1 & 0xffffffff), (3 & 0xffffffff)));
test__shadeup_assert(__swiz_cross_int4_2_0_2_0(__int4___make_vec((1 & 0xffffffff), (2 & 0xffffffff), (3 & 0xffffffff), (4 & 0xffffffff))), __int4___make_vec((4 & 0xffffffff), (1 & 0xffffffff), (4 & 0xffffffff), (1 & 0xffffffff)));
test__shadeup_assert(__swiz_cross_int4_0_1_2(__int4___make_vec((1 & 0xffffffff), (2 & 0xffffffff), (3 & 0xffffffff), (4 & 0xffffffff))), __int3___make_vec((1 & 0xffffffff), (2 & 0xffffffff), (3 & 0xffffffff)));

}

function __swiz_cross_int3_1_0_2(a) {
return [a[1], a[0], a[2]];
}

function __int4___make_vec(x, y, z, w) {
return [x, y, z, w];
}

function __swiz_cross_int2_0_1(a) {
return [a[0], a[1]];
}

function __swiz_cross_int3_0_1(a) {
return [a[0], a[1]];
}

function __swiz_cross_int2_0(a) {
return a[0];
}

function __swiz_cross_int3_0_1_2(a) {
return [a[0], a[1], a[2]];
}

function __swiz_cross_int4_1(a) {
return a[1];
}

function __int2___make_vec(x, y) {
return [x, y];
}

function __int3___make_vec(x, y, z) {
return [x, y, z];
}

function __swiz_cross_int2_1_0(a) {
return [a[1], a[0]];
}

function __swiz_cross_int3_1_0(a) {
return [a[1], a[0]];
}

function __swiz_cross_int4_2_0_2_0(a) {
return [a[2], a[0], a[2], a[0]];
}

function __swiz_cross_int4_0_1_2(a) {
return [a[0], a[1], a[2]];
}

function __swiz_cross_int4_3(a) {
return a[3];
}

function __swiz_cross_int3_1(a) {
return a[1];
}

function __swiz_cross_int3_1_2(a) {
return [a[1], a[2]];
}


Parse: 356.8015ms
Process: 14.0775ms
Generate: 71.3981ms
Total: 444.8795ms
