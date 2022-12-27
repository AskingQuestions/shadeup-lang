const __SHADERS = [
  __shadeup_gen_shader({z: 'array<test__shadeup_Light>'}, `struct test_i_shadeup_Light {
vec3 pos;
vec3 color;

};
test_i_shadeup_Light _i_make_struct_test_i_shadeup_Light(vec3 color, vec3 pos) {
test_i_shadeup_Light _i_struct;
_i_struct.pos = pos;
_i_struct.color = color;

return _i_struct;
}
vec3 _i_get_struct_test_i_shadeup_Light_pos(test_i_shadeup_Light _i_struct) {
return _i_struct.pos;
}
vec3 _i_get_struct_test_i_shadeup_Light_color(test_i_shadeup_Light _i_struct) {
return _i_struct.color;
}


uniform test_i_shadeup_Light _i_in_z[%z_size%];
uniform int _i_in_z_size;


ivec4 primitives_int4_method_ii_make_vec(int x, int y, int z, int w) {
return ivec4(x, y, z, w);}
vec4 primitives_float4_method_ii_cast_from_vec_float4(vec4 other) {
return vec4(0, 0, 0, 0);}
/*__SHADEUP_TEMPLATE_INSERT_MAIN_BEFORE__*/
void main() {
/*__SHADEUP_TEMPLATE_INSERT_MAIN_START__*/
_i_in_z_size;
pixel = vec4(ivec4(0, 1, 0, 1));

/*__SHADEUP_TEMPLATE_INSERT_MAIN_END__*/
}
`),

];

__shadeup_register_struct('test__shadeup_Light', {pos: 'float3', color: 'float3'});
/* cpu_only can except */function primitives_draw_shader(__stack_info, shade) {
try {
__shadeup_dispatch_draw(shade)} catch (e) {
  throw __shadeup_error(e, __stack_info);
}

}

window['primitives_draw_shader'] = primitives_draw_shader;
function primitives_float_method___cast_from_scalar_float(other) {
return other;
}

window['primitives_float_method___cast_from_scalar_float'] = primitives_float_method___cast_from_scalar_float;
function primitives_float_method___operator_multiply_float_float(__this, other) {
return (__this * other);
}

window['primitives_float_method___operator_multiply_float_float'] = primitives_float_method___operator_multiply_float_float;
function primitives_int3_method___make_vec(x, y, z) {
return [x, y, z];
}

window['primitives_int3_method___make_vec'] = primitives_int3_method___make_vec;
function __make_struct_test__shadeup_Light(fields) {
return {pos: fields.pos, color: fields.color, };
}

window['__make_struct_test__shadeup_Light'] = __make_struct_test__shadeup_Light;
/* cpu_only can except */function test__shadeup_main(__stack_info) {
try {
let l = __make_struct_test__shadeup_Light({pos: primitives_float3_method___cast_from_vec_float3(primitives_int3_method___make_vec((1 & 0xffffffff), (2 & 0xffffffff), (3 & 0xffffffff))), color: primitives_float3_method___cast_from_vec_float3(primitives_int3_method___make_vec((1 & 0xffffffff), (1 & 0xffffffff), (1 & 0xffffffff)))});
let z = [l];
let yy = __swiz_cross_float3_0(z[(0 & 0xffffffff)].color);
let x = __SHADERS[0].instance({z: z});
let a = primitives_float_method___operator_multiply_float_float(primitives_float_method___cast_from_scalar_float((1 & 0xffffffff)), 1.2);
primitives_draw_shader(['primitives_draw_shader', 293, 300], x);
} catch (e) {
  throw __shadeup_error(e, __stack_info);
}

}

window['test__shadeup_main'] = test__shadeup_main;
function __swiz_cross_float3_0(a) {
return a[0];
}

window['__swiz_cross_float3_0'] = __swiz_cross_float3_0;
function other__shadeup___init_file() {
let glob = "Hi!";
let starter = 1;
window['_shadeup_file_globals_other__shadeup_glob'] = glob;
window['_shadeup_file_globals_other__shadeup_starter'] = starter;

}

window['other__shadeup___init_file'] = other__shadeup___init_file;
function test__shadeup___init_file() {
let g = [(1 & 0xffffffff), (2 & 0xffffffff), (3 & 0xffffffff)];
window['_shadeup_file_globals_test__shadeup_g'] = g;

}

window['test__shadeup___init_file'] = test__shadeup___init_file;
function primitives_float3_method___cast_from_vec_float3(other) {
return [(other[0]), (other[1]), (other[2])];
}

window['primitives_float3_method___cast_from_vec_float3'] = primitives_float3_method___cast_from_vec_float3;

Parse: 28.859ms
Process: 5.3808ms
Generate: 77.3831ms
Total: 112.0336ms
