const __SHADERS = [

];

function primitives_int4_method___operator_plus_int4_int(__this, other) {
return [(__this[0] + other) & 0xffffffff, (__this[1] + other) & 0xffffffff, (__this[2] + other) & 0xffffffff, (__this[3] + other) & 0xffffffff]
}

window['primitives_int4_method___operator_plus_int4_int'] = primitives_int4_method___operator_plus_int4_int;
function primitives_half_method___postfix_operator_join_half(__this) {
return (__this++);
}

window['primitives_half_method___postfix_operator_join_half'] = primitives_half_method___postfix_operator_join_half;
function primitives_half4_method___cast_from_vec_half4(other) {
return [(other[0]), (other[1]), (other[2]), (other[3])];
}

window['primitives_half4_method___cast_from_vec_half4'] = primitives_half4_method___cast_from_vec_half4;
function primitives_int_method___operator_double_multiply_int_int(__this, other) {
return (__this ** other) & 0xffffffff;
}

window['primitives_int_method___operator_double_multiply_int_int'] = primitives_int_method___operator_double_multiply_int_int;
function primitives_uint4_method___operator_double_multiply_uint4_uint4(__this, other) {
return [(__this[0] ** other[0])>>>0 & 0xffffffff, (__this[1] ** other[1])>>>0 & 0xffffffff, (__this[2] ** other[2])>>>0 & 0xffffffff, (__this[3] ** other[3])>>>0 & 0xffffffff]
}

window['primitives_uint4_method___operator_double_multiply_uint4_uint4'] = primitives_uint4_method___operator_double_multiply_uint4_uint4;
function primitives_half2_method___is_vec_2_half2_half2(__this, other) {

}

window['primitives_half2_method___is_vec_2_half2_half2'] = primitives_half2_method___is_vec_2_half2_half2;
function primitives_half4_method___operator_modulo_half4_half4(__this, other) {
return [(__this[0] % other[0]), (__this[1] % other[1]), (__this[2] % other[2]), (__this[3] % other[3])]
}

window['primitives_half4_method___operator_modulo_half4_half4'] = primitives_half4_method___operator_modulo_half4_half4;
function primitives_int2_method___operator_modulo_int2_int2(__this, other) {
return [(__this[0] % other[0]) & 0xffffffff, (__this[1] % other[1]) & 0xffffffff]
}

window['primitives_int2_method___operator_modulo_int2_int2'] = primitives_int2_method___operator_modulo_int2_int2;
function primitives_short3_method___operator_multiply_short3_short(__this, other) {
return [(__this[0] * other) & 0xffff, (__this[1] * other) & 0xffff, (__this[2] * other) & 0xffff]
}

window['primitives_short3_method___operator_multiply_short3_short'] = primitives_short3_method___operator_multiply_short3_short;
function primitives_float2_method___construct_float_float(x, y) {
return [x, y];
}

window['primitives_float2_method___construct_float_float'] = primitives_float2_method___construct_float_float;
function primitives_uint3_method___operator_minus_uint3_uint3(__this, other) {
return [(__this[0] - other[0])>>>0 & 0xffffffff, (__this[1] - other[1])>>>0 & 0xffffffff, (__this[2] - other[2])>>>0 & 0xffffffff]
}

window['primitives_uint3_method___operator_minus_uint3_uint3'] = primitives_uint3_method___operator_minus_uint3_uint3;
function primitives_uint_method___is_scalar_uint_uint(__this, other) {

}

window['primitives_uint_method___is_scalar_uint_uint'] = primitives_uint_method___is_scalar_uint_uint;
function primitives_short4_method___operator_divide_short4_short(__this, other) {
return [(__this[0] / other) & 0xffff, (__this[1] / other) & 0xffff, (__this[2] / other) & 0xffff, (__this[3] / other) & 0xffff]
}

window['primitives_short4_method___operator_divide_short4_short'] = primitives_short4_method___operator_divide_short4_short;
function primitives_uint3_method___operator_modulo_uint3_uint3(__this, other) {
return [(__this[0] % other[0])>>>0 & 0xffffffff, (__this[1] % other[1])>>>0 & 0xffffffff, (__this[2] % other[2])>>>0 & 0xffffffff]
}

window['primitives_uint3_method___operator_modulo_uint3_uint3'] = primitives_uint3_method___operator_modulo_uint3_uint3;
function primitives_short4_method___operator_double_multiply_short4_short(__this, other) {
return [(__this[0] ** other) & 0xffff, (__this[1] ** other) & 0xffff, (__this[2] ** other) & 0xffff, (__this[3] ** other) & 0xffff]
}

window['primitives_short4_method___operator_double_multiply_short4_short'] = primitives_short4_method___operator_double_multiply_short4_short;
function primitives_double4_method___cast_from_vec_double4(other) {
return [(other[0]), (other[1]), (other[2]), (other[3])];
}

window['primitives_double4_method___cast_from_vec_double4'] = primitives_double4_method___cast_from_vec_double4;
function primitives_half3_method___operator_plus_half3_half3(__this, other) {
return [(__this[0] + other[0]), (__this[1] + other[1]), (__this[2] + other[2])]
}

window['primitives_half3_method___operator_plus_half3_half3'] = primitives_half3_method___operator_plus_half3_half3;
function primitives_int2_method___operator_multiply_int2_int(__this, other) {
return [(__this[0] * other) & 0xffffffff, (__this[1] * other) & 0xffffffff]
}

window['primitives_int2_method___operator_multiply_int2_int'] = primitives_int2_method___operator_multiply_int2_int;
function primitives_short2_method___construct_short_short(x, y) {
return [x, y];
}

window['primitives_short2_method___construct_short_short'] = primitives_short2_method___construct_short_short;
function primitives_float2_method___operator_double_multiply_float2_float(__this, other) {
return [(__this[0] ** other), (__this[1] ** other)]
}

window['primitives_float2_method___operator_double_multiply_float2_float'] = primitives_float2_method___operator_double_multiply_float2_float;
function primitives_int_method___operator_plus_int_int(__this, other) {
return (__this + other) & 0xffffffff;
}

window['primitives_int_method___operator_plus_int_int'] = primitives_int_method___operator_plus_int_int;
function primitives_uint4_method___operator_divide_uint4_uint(__this, other) {
return [(__this[0] / other)>>>0 & 0xffffffff, (__this[1] / other)>>>0 & 0xffffffff, (__this[2] / other)>>>0 & 0xffffffff, (__this[3] / other)>>>0 & 0xffffffff]
}

window['primitives_uint4_method___operator_divide_uint4_uint'] = primitives_uint4_method___operator_divide_uint4_uint;
function primitives_short3_method___make_vec(x, y, z) {
return [x, y, z];
}

window['primitives_short3_method___make_vec'] = primitives_short3_method___make_vec;
function primitives_float3_method___operator_plus_float3_float(__this, other) {
return [(__this[0] + other), (__this[1] + other), (__this[2] + other)]
}

window['primitives_float3_method___operator_plus_float3_float'] = primitives_float3_method___operator_plus_float3_float;
function primitives_int4_method___operator_double_multiply_int4_int4(__this, other) {
return [(__this[0] ** other[0]) & 0xffffffff, (__this[1] ** other[1]) & 0xffffffff, (__this[2] ** other[2]) & 0xffffffff, (__this[3] ** other[3]) & 0xffffffff]
}

window['primitives_int4_method___operator_double_multiply_int4_int4'] = primitives_int4_method___operator_double_multiply_int4_int4;
function primitives_int2_method___operator_minus_int2_int(__this, other) {
return [(__this[0] - other) & 0xffffffff, (__this[1] - other) & 0xffffffff]
}

window['primitives_int2_method___operator_minus_int2_int'] = primitives_int2_method___operator_minus_int2_int;
function primitives_uint_method___operator_bar_uint_uint(__this, other) {
return (__this | other)>>>0 & 0xffffffff;
}

window['primitives_uint_method___operator_bar_uint_uint'] = primitives_uint_method___operator_bar_uint_uint;
function primitives_int_method___operator_greater_than_int_int(__this, other) {
return __this > other;
}

window['primitives_int_method___operator_greater_than_int_int'] = primitives_int_method___operator_greater_than_int_int;
function primitives_short_method___prefix_operator_tilda_short(__this) {
return (~__this) & 0xffff;
}

window['primitives_short_method___prefix_operator_tilda_short'] = primitives_short_method___prefix_operator_tilda_short;
function primitives_int_method___prefix_operator_join_int(__this) {
return (++__this);
}

window['primitives_int_method___prefix_operator_join_int'] = primitives_int_method___prefix_operator_join_int;
function primitives_double3_method___prefix_operator_minus_double3(__this) {
return [(-__this[0]), (-__this[1]), (-__this[2])]
}

window['primitives_double3_method___prefix_operator_minus_double3'] = primitives_double3_method___prefix_operator_minus_double3;
function primitives_float_method___operator_plus_float_float(__this, other) {
return (__this + other);
}

window['primitives_float_method___operator_plus_float_float'] = primitives_float_method___operator_plus_float_float;
function primitives_uint_method___prefix_operator_tilda_uint(__this) {
return (~__this)>>>0 & 0xffffffff;
}

window['primitives_uint_method___prefix_operator_tilda_uint'] = primitives_uint_method___prefix_operator_tilda_uint;
function primitives_uint3_method___operator_divide_uint3_uint3(__this, other) {
return [(__this[0] / other[0])>>>0 & 0xffffffff, (__this[1] / other[1])>>>0 & 0xffffffff, (__this[2] / other[2])>>>0 & 0xffffffff]
}

window['primitives_uint3_method___operator_divide_uint3_uint3'] = primitives_uint3_method___operator_divide_uint3_uint3;
function primitives_double4_method___operator_multiply_double4_double(__this, other) {
return [(__this[0] * other), (__this[1] * other), (__this[2] * other), (__this[3] * other)]
}

window['primitives_double4_method___operator_multiply_double4_double'] = primitives_double4_method___operator_multiply_double4_double;
function primitives_short4_method___operator_multiply_short4_short(__this, other) {
return [(__this[0] * other) & 0xffff, (__this[1] * other) & 0xffff, (__this[2] * other) & 0xffff, (__this[3] * other) & 0xffff]
}

window['primitives_short4_method___operator_multiply_short4_short'] = primitives_short4_method___operator_multiply_short4_short;
function primitives_int3_method___construct_int_int_int(x, y, z) {
return [x, y, z];
}

window['primitives_int3_method___construct_int_int_int'] = primitives_int3_method___construct_int_int_int;
function primitives_float3_method___operator_modulo_float3_float3(__this, other) {
return [(__this[0] % other[0]), (__this[1] % other[1]), (__this[2] % other[2])]
}

window['primitives_float3_method___operator_modulo_float3_float3'] = primitives_float3_method___operator_modulo_float3_float3;
function primitives_int_method___prefix_operator_minus_int(__this) {
return (-__this) & 0xffffffff;
}

window['primitives_int_method___prefix_operator_minus_int'] = primitives_int_method___prefix_operator_minus_int;
function primitives_bool_method___operator_bar_bar_bool_bool(__this, other) {
return __this || other;
}

window['primitives_bool_method___operator_bar_bar_bool_bool'] = primitives_bool_method___operator_bar_bar_bool_bool;
function primitives_int2_method___make_vec(x, y) {
return [x, y];
}

window['primitives_int2_method___make_vec'] = primitives_int2_method___make_vec;
function primitives_short3_method___operator_double_multiply_short3_short3(__this, other) {
return [(__this[0] ** other[0]) & 0xffff, (__this[1] ** other[1]) & 0xffff, (__this[2] ** other[2]) & 0xffff]
}

window['primitives_short3_method___operator_double_multiply_short3_short3'] = primitives_short3_method___operator_double_multiply_short3_short3;
function primitives_uint3_method___operator_double_multiply_uint3_uint3(__this, other) {
return [(__this[0] ** other[0])>>>0 & 0xffffffff, (__this[1] ** other[1])>>>0 & 0xffffffff, (__this[2] ** other[2])>>>0 & 0xffffffff]
}

window['primitives_uint3_method___operator_double_multiply_uint3_uint3'] = primitives_uint3_method___operator_double_multiply_uint3_uint3;
function primitives_half3_method___operator_cross_half3_half3(__this, other) {
return [(__this[0] * other[0]), (__this[1] * other[1]), (__this[2] * other[2])]
}

window['primitives_half3_method___operator_cross_half3_half3'] = primitives_half3_method___operator_cross_half3_half3;
function primitives_uint4_method___operator_divide_uint4_uint4(__this, other) {
return [(__this[0] / other[0])>>>0 & 0xffffffff, (__this[1] / other[1])>>>0 & 0xffffffff, (__this[2] / other[2])>>>0 & 0xffffffff, (__this[3] / other[3])>>>0 & 0xffffffff]
}

window['primitives_uint4_method___operator_divide_uint4_uint4'] = primitives_uint4_method___operator_divide_uint4_uint4;
function primitives_uint2_method___operator_minus_uint2_uint2(__this, other) {
return [(__this[0] - other[0])>>>0 & 0xffffffff, (__this[1] - other[1])>>>0 & 0xffffffff]
}

window['primitives_uint2_method___operator_minus_uint2_uint2'] = primitives_uint2_method___operator_minus_uint2_uint2;
function primitives_short_method___operator_hat_short_short(__this, other) {
return (__this ^ other) & 0xffff;
}

window['primitives_short_method___operator_hat_short_short'] = primitives_short_method___operator_hat_short_short;
function primitives_float4_method___operator_cross_float4_float4(__this, other) {
return [(__this[0] * other[0]), (__this[1] * other[1]), (__this[2] * other[2]), (__this[3] * other[3])]
}

window['primitives_float4_method___operator_cross_float4_float4'] = primitives_float4_method___operator_cross_float4_float4;
function primitives_short_method___operator_plus_short_short(__this, other) {
return (__this + other) & 0xffff;
}

window['primitives_short_method___operator_plus_short_short'] = primitives_short_method___operator_plus_short_short;
function primitives_half2_method___operator_double_multiply_half2_half2(__this, other) {
return [(__this[0] ** other[0]), (__this[1] ** other[1])]
}

window['primitives_half2_method___operator_double_multiply_half2_half2'] = primitives_half2_method___operator_double_multiply_half2_half2;
function primitives_double4_method___prefix_operator_minus_double4(__this) {
return [(-__this[0]), (-__this[1]), (-__this[2]), (-__this[3])]
}

window['primitives_double4_method___prefix_operator_minus_double4'] = primitives_double4_method___prefix_operator_minus_double4;
function primitives_half_method___operator_plus_half_half(__this, other) {
return (__this + other);
}

window['primitives_half_method___operator_plus_half_half'] = primitives_half_method___operator_plus_half_half;
function primitives_short4_method___operator_minus_short4_short4(__this, other) {
return [(__this[0] - other[0]) & 0xffff, (__this[1] - other[1]) & 0xffff, (__this[2] - other[2]) & 0xffff, (__this[3] - other[3]) & 0xffff]
}

window['primitives_short4_method___operator_minus_short4_short4'] = primitives_short4_method___operator_minus_short4_short4;
function primitives_uint3_method___operator_double_multiply_uint3_uint(__this, other) {
return [(__this[0] ** other)>>>0 & 0xffffffff, (__this[1] ** other)>>>0 & 0xffffffff, (__this[2] ** other)>>>0 & 0xffffffff]
}

window['primitives_uint3_method___operator_double_multiply_uint3_uint'] = primitives_uint3_method___operator_double_multiply_uint3_uint;
function primitives_half3_method___operator_multiply_half3_half(__this, other) {
return [(__this[0] * other), (__this[1] * other), (__this[2] * other)]
}

window['primitives_half3_method___operator_multiply_half3_half'] = primitives_half3_method___operator_multiply_half3_half;
function primitives_short2_method___operator_double_multiply_short2_short2(__this, other) {
return [(__this[0] ** other[0]) & 0xffff, (__this[1] ** other[1]) & 0xffff]
}

window['primitives_short2_method___operator_double_multiply_short2_short2'] = primitives_short2_method___operator_double_multiply_short2_short2;
function primitives_dist_uint4_uint4(a, b) {
return Math.sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y) + (a.z - b.z) * (a.z - b.z) + (a.w - b.w) * (a.w - b.w));
}

window['primitives_dist_uint4_uint4'] = primitives_dist_uint4_uint4;
function primitives_short3_method___construct_short_short_short(x, y, z) {
return [x, y, z];
}

window['primitives_short3_method___construct_short_short_short'] = primitives_short3_method___construct_short_short_short;
function primitives_half3_method___is_vec_3_half3_half3(__this, other) {

}

window['primitives_half3_method___is_vec_3_half3_half3'] = primitives_half3_method___is_vec_3_half3_half3;
function primitives_int3_method___operator_double_multiply_int3_int(__this, other) {
return [(__this[0] ** other) & 0xffffffff, (__this[1] ** other) & 0xffffffff, (__this[2] ** other) & 0xffffffff]
}

window['primitives_int3_method___operator_double_multiply_int3_int'] = primitives_int3_method___operator_double_multiply_int3_int;
function primitives_dist_int_int(a, b) {
return Math.abs(a - b);
}

window['primitives_dist_int_int'] = primitives_dist_int_int;
function primitives_float_method___operator_minus_float_float(__this, other) {
return (__this - other);
}

window['primitives_float_method___operator_minus_float_float'] = primitives_float_method___operator_minus_float_float;
function primitives_dist_double3_double3(a, b) {
return Math.sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y) + (a.z - b.z) * (a.z - b.z));
}

window['primitives_dist_double3_double3'] = primitives_dist_double3_double3;
function primitives_short2_method___operator_plus_short2_short2(__this, other) {
return [(__this[0] + other[0]) & 0xffff, (__this[1] + other[1]) & 0xffff]
}

window['primitives_short2_method___operator_plus_short2_short2'] = primitives_short2_method___operator_plus_short2_short2;
function primitives_int_method___operator_double_left_int_int(__this, other) {
return (__this << other) & 0xffffffff;
}

window['primitives_int_method___operator_double_left_int_int'] = primitives_int_method___operator_double_left_int_int;
function primitives_dist_float2_float2(a, b) {
return dist(a, b);
}

window['primitives_dist_float2_float2'] = primitives_dist_float2_float2;
function primitives_uint_method___operator_less_than_or_equals_uint_uint(__this, other) {
return __this <= other;
}

window['primitives_uint_method___operator_less_than_or_equals_uint_uint'] = primitives_uint_method___operator_less_than_or_equals_uint_uint;
function primitives_double3_method___operator_minus_double3_double(__this, other) {
return [(__this[0] - other), (__this[1] - other), (__this[2] - other)]
}

window['primitives_double3_method___operator_minus_double3_double'] = primitives_double3_method___operator_minus_double3_double;
function primitives_half2_method___operator_double_multiply_half2_half(__this, other) {
return [(__this[0] ** other), (__this[1] ** other)]
}

window['primitives_half2_method___operator_double_multiply_half2_half'] = primitives_half2_method___operator_double_multiply_half2_half;
function primitives_float_method___prefix_operator_join_float(__this) {
return (++__this);
}

window['primitives_float_method___prefix_operator_join_float'] = primitives_float_method___prefix_operator_join_float;
function test__shadeup_main() {
let p = __make_struct_test__shadeup_Player({name: "Player", health: primitives_int_method___cast_from_scalar_int(100.2)});

}

window['test__shadeup_main'] = test__shadeup_main;
function primitives_int_method___operator_less_than_or_equals_int_int(__this, other) {
return __this <= other;
}

window['primitives_int_method___operator_less_than_or_equals_int_int'] = primitives_int_method___operator_less_than_or_equals_int_int;
function primitives_bool_method___prefix_operator_not_bool(__this) {
return !__this;
}

window['primitives_bool_method___prefix_operator_not_bool'] = primitives_bool_method___prefix_operator_not_bool;
function primitives_uint4_method___make_vec(x, y, z, w) {
return [x, y, z, w];
}

window['primitives_uint4_method___make_vec'] = primitives_uint4_method___make_vec;
function primitives_short2_method___prefix_operator_minus_short2(__this) {
return [(-__this[0]) & 0xffff, (-__this[1]) & 0xffff]
}

window['primitives_short2_method___prefix_operator_minus_short2'] = primitives_short2_method___prefix_operator_minus_short2;
function primitives_short2_method___operator_multiply_short2_short(__this, other) {
return [(__this[0] * other) & 0xffff, (__this[1] * other) & 0xffff]
}

window['primitives_short2_method___operator_multiply_short2_short'] = primitives_short2_method___operator_multiply_short2_short;
function primitives_double3_method___is_vec_3_double3_double3(__this, other) {

}

window['primitives_double3_method___is_vec_3_double3_double3'] = primitives_double3_method___is_vec_3_double3_double3;
function primitives_float4_method___operator_modulo_float4_float(__this, other) {
return [(__this[0] % other), (__this[1] % other), (__this[2] % other), (__this[3] % other)]
}

window['primitives_float4_method___operator_modulo_float4_float'] = primitives_float4_method___operator_modulo_float4_float;
function primitives_string_method___operator_equals_string_string(__this, other) {
return __this === other;
}

window['primitives_string_method___operator_equals_string_string'] = primitives_string_method___operator_equals_string_string;
function primitives_short_method___cast_from_scalar_short(other) {
return other & 0xffff;
}

window['primitives_short_method___cast_from_scalar_short'] = primitives_short_method___cast_from_scalar_short;
function primitives_short3_method___cast_from_vec_short3(other) {
return [(other[0] & 0xffff), (other[1] & 0xffff), (other[2] & 0xffff)];
}

window['primitives_short3_method___cast_from_vec_short3'] = primitives_short3_method___cast_from_vec_short3;
function primitives_int2_method___operator_modulo_int2_int(__this, other) {
return [(__this[0] % other) & 0xffffffff, (__this[1] % other) & 0xffffffff]
}

window['primitives_int2_method___operator_modulo_int2_int'] = primitives_int2_method___operator_modulo_int2_int;
function primitives_float2_method___operator_cross_float2_float2(__this, other) {
return [(__this[0] * other[0]), (__this[1] * other[1])]
}

window['primitives_float2_method___operator_cross_float2_float2'] = primitives_float2_method___operator_cross_float2_float2;
function primitives_double2_method___operator_divide_double2_double2(__this, other) {
return [(__this[0] / other[0]), (__this[1] / other[1])]
}

window['primitives_double2_method___operator_divide_double2_double2'] = primitives_double2_method___operator_divide_double2_double2;
function primitives_short4_method___operator_modulo_short4_short(__this, other) {
return [(__this[0] % other) & 0xffff, (__this[1] % other) & 0xffff, (__this[2] % other) & 0xffff, (__this[3] % other) & 0xffff]
}

window['primitives_short4_method___operator_modulo_short4_short'] = primitives_short4_method___operator_modulo_short4_short;
function primitives_uint3_method___cast_from_vec_uint3(other) {
return [(other[0]>>>0 & 0xffffffff), (other[1]>>>0 & 0xffffffff), (other[2]>>>0 & 0xffffffff)];
}

window['primitives_uint3_method___cast_from_vec_uint3'] = primitives_uint3_method___cast_from_vec_uint3;
function primitives_float4_method___operator_double_multiply_float4_float4(__this, other) {
return [(__this[0] ** other[0]), (__this[1] ** other[1]), (__this[2] ** other[2]), (__this[3] ** other[3])]
}

window['primitives_float4_method___operator_double_multiply_float4_float4'] = primitives_float4_method___operator_double_multiply_float4_float4;
/* cpu_only */function primitives_print_string(value) {
console.log(value)
}

window['primitives_print_string'] = primitives_print_string;
function primitives_short2_method___operator_divide_short2_short2(__this, other) {
return [(__this[0] / other[0]) & 0xffff, (__this[1] / other[1]) & 0xffff]
}

window['primitives_short2_method___operator_divide_short2_short2'] = primitives_short2_method___operator_divide_short2_short2;
function primitives_half4_method___operator_plus_half4_half4(__this, other) {
return [(__this[0] + other[0]), (__this[1] + other[1]), (__this[2] + other[2]), (__this[3] + other[3])]
}

window['primitives_half4_method___operator_plus_half4_half4'] = primitives_half4_method___operator_plus_half4_half4;
function primitives_double4_method___is_vec_4_double4_double4(__this, other) {

}

window['primitives_double4_method___is_vec_4_double4_double4'] = primitives_double4_method___is_vec_4_double4_double4;
function primitives_uint_method___operator_greater_than_or_equals_uint_uint(__this, other) {
return __this >= other;
}

window['primitives_uint_method___operator_greater_than_or_equals_uint_uint'] = primitives_uint_method___operator_greater_than_or_equals_uint_uint;
function primitives_short3_method___operator_divide_short3_short3(__this, other) {
return [(__this[0] / other[0]) & 0xffff, (__this[1] / other[1]) & 0xffff, (__this[2] / other[2]) & 0xffff]
}

window['primitives_short3_method___operator_divide_short3_short3'] = primitives_short3_method___operator_divide_short3_short3;
function primitives_int2_method___operator_cross_int2_int2(__this, other) {
return [(__this[0] * other[0]) & 0xffffffff, (__this[1] * other[1]) & 0xffffffff]
}

window['primitives_int2_method___operator_cross_int2_int2'] = primitives_int2_method___operator_cross_int2_int2;
function primitives_short4_method___operator_plus_short4_short(__this, other) {
return [(__this[0] + other) & 0xffff, (__this[1] + other) & 0xffff, (__this[2] + other) & 0xffff, (__this[3] + other) & 0xffff]
}

window['primitives_short4_method___operator_plus_short4_short'] = primitives_short4_method___operator_plus_short4_short;
function primitives_string_method___operator_plus_string_string(__this, other) {
return __this + other;
}

window['primitives_string_method___operator_plus_string_string'] = primitives_string_method___operator_plus_string_string;
function primitives_half4_method___operator_minus_half4_half4(__this, other) {
return [(__this[0] - other[0]), (__this[1] - other[1]), (__this[2] - other[2]), (__this[3] - other[3])]
}

window['primitives_half4_method___operator_minus_half4_half4'] = primitives_half4_method___operator_minus_half4_half4;
function primitives_half_method___operator_minus_half_half(__this, other) {
return (__this - other);
}

window['primitives_half_method___operator_minus_half_half'] = primitives_half_method___operator_minus_half_half;
function primitives_int4_method___operator_modulo_int4_int4(__this, other) {
return [(__this[0] % other[0]) & 0xffffffff, (__this[1] % other[1]) & 0xffffffff, (__this[2] % other[2]) & 0xffffffff, (__this[3] % other[3]) & 0xffffffff]
}

window['primitives_int4_method___operator_modulo_int4_int4'] = primitives_int4_method___operator_modulo_int4_int4;
function primitives_dist_uint_uint(a, b) {
return Math.abs(a - b);
}

window['primitives_dist_uint_uint'] = primitives_dist_uint_uint;
function primitives_float2_method___operator_divide_float2_float2(__this, other) {
return [(__this[0] / other[0]), (__this[1] / other[1])]
}

window['primitives_float2_method___operator_divide_float2_float2'] = primitives_float2_method___operator_divide_float2_float2;
function primitives_uint4_method___operator_cross_uint4_uint4(__this, other) {
return [(__this[0] * other[0])>>>0 & 0xffffffff, (__this[1] * other[1])>>>0 & 0xffffffff, (__this[2] * other[2])>>>0 & 0xffffffff, (__this[3] * other[3])>>>0 & 0xffffffff]
}

window['primitives_uint4_method___operator_cross_uint4_uint4'] = primitives_uint4_method___operator_cross_uint4_uint4;
function primitives_uint4_method___cast_from_vec_uint4(other) {
return [(other[0]>>>0 & 0xffffffff), (other[1]>>>0 & 0xffffffff), (other[2]>>>0 & 0xffffffff), (other[3]>>>0 & 0xffffffff)];
}

window['primitives_uint4_method___cast_from_vec_uint4'] = primitives_uint4_method___cast_from_vec_uint4;
function primitives_double_method___operator_minus_double_double(__this, other) {
return (__this - other);
}

window['primitives_double_method___operator_minus_double_double'] = primitives_double_method___operator_minus_double_double;
/* cpu_only async */async function primitives_sleep_float(value) {
await new Promise((res, resole) => {setTimeout(res, value * 1000)})
}

window['primitives_sleep_float'] = primitives_sleep_float;
function primitives_int2_method___operator_double_multiply_int2_int(__this, other) {
return [(__this[0] ** other) & 0xffffffff, (__this[1] ** other) & 0xffffffff]
}

window['primitives_int2_method___operator_double_multiply_int2_int'] = primitives_int2_method___operator_double_multiply_int2_int;
function primitives_double3_method___operator_multiply_double3_double(__this, other) {
return [(__this[0] * other), (__this[1] * other), (__this[2] * other)]
}

window['primitives_double3_method___operator_multiply_double3_double'] = primitives_double3_method___operator_multiply_double3_double;
function primitives_half_method___operator_divide_half_half(__this, other) {
return (__this / other);
}

window['primitives_half_method___operator_divide_half_half'] = primitives_half_method___operator_divide_half_half;
function primitives_float4_method___operator_divide_float4_float4(__this, other) {
return [(__this[0] / other[0]), (__this[1] / other[1]), (__this[2] / other[2]), (__this[3] / other[3])]
}

window['primitives_float4_method___operator_divide_float4_float4'] = primitives_float4_method___operator_divide_float4_float4;
function primitives_half2_method___operator_modulo_half2_half2(__this, other) {
return [(__this[0] % other[0]), (__this[1] % other[1])]
}

window['primitives_half2_method___operator_modulo_half2_half2'] = primitives_half2_method___operator_modulo_half2_half2;
function primitives_float2_method___operator_plus_float2_float2(__this, other) {
return [(__this[0] + other[0]), (__this[1] + other[1])]
}

window['primitives_float2_method___operator_plus_float2_float2'] = primitives_float2_method___operator_plus_float2_float2;
function primitives_float2_method___operator_minus_float2_float2(__this, other) {
return [(__this[0] - other[0]), (__this[1] - other[1])]
}

window['primitives_float2_method___operator_minus_float2_float2'] = primitives_float2_method___operator_minus_float2_float2;
function primitives_short2_method___operator_modulo_short2_short(__this, other) {
return [(__this[0] % other) & 0xffff, (__this[1] % other) & 0xffff]
}

window['primitives_short2_method___operator_modulo_short2_short'] = primitives_short2_method___operator_modulo_short2_short;
function primitives_int3_method___operator_modulo_int3_int3(__this, other) {
return [(__this[0] % other[0]) & 0xffffffff, (__this[1] % other[1]) & 0xffffffff, (__this[2] % other[2]) & 0xffffffff]
}

window['primitives_int3_method___operator_modulo_int3_int3'] = primitives_int3_method___operator_modulo_int3_int3;
function primitives_half_method___prefix_operator_minus_minus_half(__this) {
return (--__this);
}

window['primitives_half_method___prefix_operator_minus_minus_half'] = primitives_half_method___prefix_operator_minus_minus_half;
function primitives_uint2_method___cast_from_vec_uint2(other) {
return [(other[0]>>>0 & 0xffffffff), (other[1]>>>0 & 0xffffffff)];
}

window['primitives_uint2_method___cast_from_vec_uint2'] = primitives_uint2_method___cast_from_vec_uint2;
function primitives_short_method___operator_greater_than_or_equals_short_short(__this, other) {
return __this >= other;
}

window['primitives_short_method___operator_greater_than_or_equals_short_short'] = primitives_short_method___operator_greater_than_or_equals_short_short;
function primitives_int2_method___construct_int_int(x, y) {
return [x, y];
}

window['primitives_int2_method___construct_int_int'] = primitives_int2_method___construct_int_int;
function primitives_double_method___prefix_operator_join_double(__this) {
return (++__this);
}

window['primitives_double_method___prefix_operator_join_double'] = primitives_double_method___prefix_operator_join_double;
function primitives_double3_method___operator_modulo_double3_double3(__this, other) {
return [(__this[0] % other[0]), (__this[1] % other[1]), (__this[2] % other[2])]
}

window['primitives_double3_method___operator_modulo_double3_double3'] = primitives_double3_method___operator_modulo_double3_double3;
function primitives_uint_method___operator_double_left_uint_uint(__this, other) {
return (__this << other)>>>0 & 0xffffffff;
}

window['primitives_uint_method___operator_double_left_uint_uint'] = primitives_uint_method___operator_double_left_uint_uint;
function primitives_float3_method___operator_double_multiply_float3_float(__this, other) {
return [(__this[0] ** other), (__this[1] ** other), (__this[2] ** other)]
}

window['primitives_float3_method___operator_double_multiply_float3_float'] = primitives_float3_method___operator_double_multiply_float3_float;
function primitives_double_method___cast_from_scalar_double(other) {
return other;
}

window['primitives_double_method___cast_from_scalar_double'] = primitives_double_method___cast_from_scalar_double;
function primitives_short4_method___operator_minus_short4_short(__this, other) {
return [(__this[0] - other) & 0xffff, (__this[1] - other) & 0xffff, (__this[2] - other) & 0xffff, (__this[3] - other) & 0xffff]
}

window['primitives_short4_method___operator_minus_short4_short'] = primitives_short4_method___operator_minus_short4_short;
function primitives_double3_method___operator_multiply_double3_double3(__this, other) {
return [(__this[0] * other[0]), (__this[1] * other[1]), (__this[2] * other[2])]
}

window['primitives_double3_method___operator_multiply_double3_double3'] = primitives_double3_method___operator_multiply_double3_double3;
function primitives_float_method___operator_greater_than_float_float(__this, other) {
return __this > other;
}

window['primitives_float_method___operator_greater_than_float_float'] = primitives_float_method___operator_greater_than_float_float;
function primitives_float3_method___operator_double_multiply_float3_float3(__this, other) {
return [(__this[0] ** other[0]), (__this[1] ** other[1]), (__this[2] ** other[2])]
}

window['primitives_float3_method___operator_double_multiply_float3_float3'] = primitives_float3_method___operator_double_multiply_float3_float3;
function primitives_uint_method___postfix_operator_join_uint(__this) {
return (__this++);
}

window['primitives_uint_method___postfix_operator_join_uint'] = primitives_uint_method___postfix_operator_join_uint;
function primitives_uint4_method___operator_plus_uint4_uint4(__this, other) {
return [(__this[0] + other[0])>>>0 & 0xffffffff, (__this[1] + other[1])>>>0 & 0xffffffff, (__this[2] + other[2])>>>0 & 0xffffffff, (__this[3] + other[3])>>>0 & 0xffffffff]
}

window['primitives_uint4_method___operator_plus_uint4_uint4'] = primitives_uint4_method___operator_plus_uint4_uint4;
function primitives_uint3_method___operator_minus_uint3_uint(__this, other) {
return [(__this[0] - other)>>>0 & 0xffffffff, (__this[1] - other)>>>0 & 0xffffffff, (__this[2] - other)>>>0 & 0xffffffff]
}

window['primitives_uint3_method___operator_minus_uint3_uint'] = primitives_uint3_method___operator_minus_uint3_uint;
function primitives_double4_method___operator_plus_double4_double4(__this, other) {
return [(__this[0] + other[0]), (__this[1] + other[1]), (__this[2] + other[2]), (__this[3] + other[3])]
}

window['primitives_double4_method___operator_plus_double4_double4'] = primitives_double4_method___operator_plus_double4_double4;
function primitives_half_method___operator_equals_half_half(__this, other) {
return __this === other;
}

window['primitives_half_method___operator_equals_half_half'] = primitives_half_method___operator_equals_half_half;
function primitives_half3_method___operator_multiply_half3_half3(__this, other) {
return [(__this[0] * other[0]), (__this[1] * other[1]), (__this[2] * other[2])]
}

window['primitives_half3_method___operator_multiply_half3_half3'] = primitives_half3_method___operator_multiply_half3_half3;
function primitives_int_method___cast_from_scalar_int(other) {
return other & 0xffffffff;
}

window['primitives_int_method___cast_from_scalar_int'] = primitives_int_method___cast_from_scalar_int;
function primitives_half2_method___operator_modulo_half2_half(__this, other) {
return [(__this[0] % other), (__this[1] % other)]
}

window['primitives_half2_method___operator_modulo_half2_half'] = primitives_half2_method___operator_modulo_half2_half;
function primitives_uint3_method___operator_multiply_uint3_uint(__this, other) {
return [(__this[0] * other)>>>0 & 0xffffffff, (__this[1] * other)>>>0 & 0xffffffff, (__this[2] * other)>>>0 & 0xffffffff]
}

window['primitives_uint3_method___operator_multiply_uint3_uint'] = primitives_uint3_method___operator_multiply_uint3_uint;
function primitives_int_method___operator_and_int_int(__this, other) {
return (__this & other) & 0xffffffff;
}

window['primitives_int_method___operator_and_int_int'] = primitives_int_method___operator_and_int_int;
function primitives_int_method___operator_greater_than_or_equals_int_int(__this, other) {
return __this >= other;
}

window['primitives_int_method___operator_greater_than_or_equals_int_int'] = primitives_int_method___operator_greater_than_or_equals_int_int;
function primitives_uint2_method___operator_divide_uint2_uint(__this, other) {
return [(__this[0] / other)>>>0 & 0xffffffff, (__this[1] / other)>>>0 & 0xffffffff]
}

window['primitives_uint2_method___operator_divide_uint2_uint'] = primitives_uint2_method___operator_divide_uint2_uint;
function primitives_uint_method___operator_multiply_uint_uint(__this, other) {
return (__this * other)>>>0 & 0xffffffff;
}

window['primitives_uint_method___operator_multiply_uint_uint'] = primitives_uint_method___operator_multiply_uint_uint;
function primitives_int3_method___operator_divide_int3_int(__this, other) {
return [(__this[0] / other) & 0xffffffff, (__this[1] / other) & 0xffffffff, (__this[2] / other) & 0xffffffff]
}

window['primitives_int3_method___operator_divide_int3_int'] = primitives_int3_method___operator_divide_int3_int;
function primitives_half4_method___make_vec(x, y, z, w) {
return [x, y, z, w];
}

window['primitives_half4_method___make_vec'] = primitives_half4_method___make_vec;
function primitives_int4_method___prefix_operator_minus_int4(__this) {
return [(-__this[0]) & 0xffffffff, (-__this[1]) & 0xffffffff, (-__this[2]) & 0xffffffff, (-__this[3]) & 0xffffffff]
}

window['primitives_int4_method___prefix_operator_minus_int4'] = primitives_int4_method___prefix_operator_minus_int4;
function primitives_uint_method___operator_minus_uint_uint(__this, other) {
return (__this - other)>>>0 & 0xffffffff;
}

window['primitives_uint_method___operator_minus_uint_uint'] = primitives_uint_method___operator_minus_uint_uint;
function primitives_half_method___operator_greater_than_half_half(__this, other) {
return __this > other;
}

window['primitives_half_method___operator_greater_than_half_half'] = primitives_half_method___operator_greater_than_half_half;
function primitives_float_method___operator_less_than_float_float(__this, other) {
return __this < other;
}

window['primitives_float_method___operator_less_than_float_float'] = primitives_float_method___operator_less_than_float_float;
function primitives_short3_method___operator_cross_short3_short3(__this, other) {
return [(__this[0] * other[0]) & 0xffff, (__this[1] * other[1]) & 0xffff, (__this[2] * other[2]) & 0xffff]
}

window['primitives_short3_method___operator_cross_short3_short3'] = primitives_short3_method___operator_cross_short3_short3;
function primitives_double_method___prefix_operator_minus_double(__this) {
return (-__this);
}

window['primitives_double_method___prefix_operator_minus_double'] = primitives_double_method___prefix_operator_minus_double;
function primitives_double2_method___operator_modulo_double2_double(__this, other) {
return [(__this[0] % other), (__this[1] % other)]
}

window['primitives_double2_method___operator_modulo_double2_double'] = primitives_double2_method___operator_modulo_double2_double;
function primitives_short3_method___operator_plus_short3_short(__this, other) {
return [(__this[0] + other) & 0xffff, (__this[1] + other) & 0xffff, (__this[2] + other) & 0xffff]
}

window['primitives_short3_method___operator_plus_short3_short'] = primitives_short3_method___operator_plus_short3_short;
function primitives_uint2_method___operator_double_multiply_uint2_uint(__this, other) {
return [(__this[0] ** other)>>>0 & 0xffffffff, (__this[1] ** other)>>>0 & 0xffffffff]
}

window['primitives_uint2_method___operator_double_multiply_uint2_uint'] = primitives_uint2_method___operator_double_multiply_uint2_uint;
function primitives_float2_method___operator_double_multiply_float2_float2(__this, other) {
return [(__this[0] ** other[0]), (__this[1] ** other[1])]
}

window['primitives_float2_method___operator_double_multiply_float2_float2'] = primitives_float2_method___operator_double_multiply_float2_float2;
function primitives_double4_method___make_vec(x, y, z, w) {
return [x, y, z, w];
}

window['primitives_double4_method___make_vec'] = primitives_double4_method___make_vec;
function primitives_float4_method___operator_plus_float4_float4(__this, other) {
return [(__this[0] + other[0]), (__this[1] + other[1]), (__this[2] + other[2]), (__this[3] + other[3])]
}

window['primitives_float4_method___operator_plus_float4_float4'] = primitives_float4_method___operator_plus_float4_float4;
function primitives_half_method___operator_less_than_half_half(__this, other) {
return __this < other;
}

window['primitives_half_method___operator_less_than_half_half'] = primitives_half_method___operator_less_than_half_half;
function primitives_short2_method___operator_modulo_short2_short2(__this, other) {
return [(__this[0] % other[0]) & 0xffff, (__this[1] % other[1]) & 0xffff]
}

window['primitives_short2_method___operator_modulo_short2_short2'] = primitives_short2_method___operator_modulo_short2_short2;
function primitives_float2_method___cast_from_vec_float2(other) {
return [(other[0]), (other[1])];
}

window['primitives_float2_method___cast_from_vec_float2'] = primitives_float2_method___cast_from_vec_float2;
function primitives_short_method___operator_double_left_short_short(__this, other) {
return (__this << other) & 0xffff;
}

window['primitives_short_method___operator_double_left_short_short'] = primitives_short_method___operator_double_left_short_short;
function primitives_half4_method___prefix_operator_minus_half4(__this) {
return [(-__this[0]), (-__this[1]), (-__this[2]), (-__this[3])]
}

window['primitives_half4_method___prefix_operator_minus_half4'] = primitives_half4_method___prefix_operator_minus_half4;
function primitives_int3_method___operator_multiply_int3_int3(__this, other) {
return [(__this[0] * other[0]) & 0xffffffff, (__this[1] * other[1]) & 0xffffffff, (__this[2] * other[2]) & 0xffffffff]
}

window['primitives_int3_method___operator_multiply_int3_int3'] = primitives_int3_method___operator_multiply_int3_int3;
function primitives_float4_method___cast_from_vec_float4(other) {
return [(other[0]), (other[1]), (other[2]), (other[3])];
}

window['primitives_float4_method___cast_from_vec_float4'] = primitives_float4_method___cast_from_vec_float4;
function primitives_short2_method___operator_multiply_short2_short2(__this, other) {
return [(__this[0] * other[0]) & 0xffff, (__this[1] * other[1]) & 0xffff]
}

window['primitives_short2_method___operator_multiply_short2_short2'] = primitives_short2_method___operator_multiply_short2_short2;
function primitives_short4_method___construct_short_short_short_short(x, y, z, w) {
return [x, y, z, w];
}

window['primitives_short4_method___construct_short_short_short_short'] = primitives_short4_method___construct_short_short_short_short;
function primitives_uint4_method___is_vec_4_uint4_uint4(__this, other) {

}

window['primitives_uint4_method___is_vec_4_uint4_uint4'] = primitives_uint4_method___is_vec_4_uint4_uint4;
function primitives_float_method___operator_greater_than_or_equals_float_float(__this, other) {
return __this >= other;
}

window['primitives_float_method___operator_greater_than_or_equals_float_float'] = primitives_float_method___operator_greater_than_or_equals_float_float;
function primitives_short3_method___is_vec_3_short3_short3(__this, other) {

}

window['primitives_short3_method___is_vec_3_short3_short3'] = primitives_short3_method___is_vec_3_short3_short3;
function primitives_int_method___operator_multiply_int_int(__this, other) {
return (__this * other) & 0xffffffff;
}

window['primitives_int_method___operator_multiply_int_int'] = primitives_int_method___operator_multiply_int_int;
function primitives_int2_method___operator_divide_int2_int(__this, other) {
return [(__this[0] / other) & 0xffffffff, (__this[1] / other) & 0xffffffff]
}

window['primitives_int2_method___operator_divide_int2_int'] = primitives_int2_method___operator_divide_int2_int;
function primitives_half4_method___operator_plus_half4_half(__this, other) {
return [(__this[0] + other), (__this[1] + other), (__this[2] + other), (__this[3] + other)]
}

window['primitives_half4_method___operator_plus_half4_half'] = primitives_half4_method___operator_plus_half4_half;
function primitives_double3_method___operator_plus_double3_double3(__this, other) {
return [(__this[0] + other[0]), (__this[1] + other[1]), (__this[2] + other[2])]
}

window['primitives_double3_method___operator_plus_double3_double3'] = primitives_double3_method___operator_plus_double3_double3;
function primitives_float3_method___construct_float_float_float(x, y, z) {
return [x, y, z];
}

window['primitives_float3_method___construct_float_float_float'] = primitives_float3_method___construct_float_float_float;
function primitives_uint3_method___prefix_operator_minus_uint3(__this) {
return [(-__this[0])>>>0 & 0xffffffff, (-__this[1])>>>0 & 0xffffffff, (-__this[2])>>>0 & 0xffffffff]
}

window['primitives_uint3_method___prefix_operator_minus_uint3'] = primitives_uint3_method___prefix_operator_minus_uint3;
function primitives_short2_method___operator_plus_short2_short(__this, other) {
return [(__this[0] + other) & 0xffff, (__this[1] + other) & 0xffff]
}

window['primitives_short2_method___operator_plus_short2_short'] = primitives_short2_method___operator_plus_short2_short;
function __get_struct_test__shadeup_Player_name(struct) {
return struct.name;
}

window['__get_struct_test__shadeup_Player_name'] = __get_struct_test__shadeup_Player_name;
function primitives_short4_method___make_vec(x, y, z, w) {
return [x, y, z, w];
}

window['primitives_short4_method___make_vec'] = primitives_short4_method___make_vec;
function primitives_double2_method___operator_cross_double2_double2(__this, other) {
return [(__this[0] * other[0]), (__this[1] * other[1])]
}

window['primitives_double2_method___operator_cross_double2_double2'] = primitives_double2_method___operator_cross_double2_double2;
function primitives_short4_method___operator_cross_short4_short4(__this, other) {
return [(__this[0] * other[0]) & 0xffff, (__this[1] * other[1]) & 0xffff, (__this[2] * other[2]) & 0xffff, (__this[3] * other[3]) & 0xffff]
}

window['primitives_short4_method___operator_cross_short4_short4'] = primitives_short4_method___operator_cross_short4_short4;
function primitives_double3_method___cast_from_vec_double3(other) {
return [(other[0]), (other[1]), (other[2])];
}

window['primitives_double3_method___cast_from_vec_double3'] = primitives_double3_method___cast_from_vec_double3;
function primitives_short4_method___prefix_operator_minus_short4(__this) {
return [(-__this[0]) & 0xffff, (-__this[1]) & 0xffff, (-__this[2]) & 0xffff, (-__this[3]) & 0xffff]
}

window['primitives_short4_method___prefix_operator_minus_short4'] = primitives_short4_method___prefix_operator_minus_short4;
function primitives_short_method___prefix_operator_minus_short(__this) {
return (-__this) & 0xffff;
}

window['primitives_short_method___prefix_operator_minus_short'] = primitives_short_method___prefix_operator_minus_short;
function primitives_dist_int2_int2(a, b) {
return dist(a, b);
}

window['primitives_dist_int2_int2'] = primitives_dist_int2_int2;
function primitives_uint4_method___operator_minus_uint4_uint(__this, other) {
return [(__this[0] - other)>>>0 & 0xffffffff, (__this[1] - other)>>>0 & 0xffffffff, (__this[2] - other)>>>0 & 0xffffffff, (__this[3] - other)>>>0 & 0xffffffff]
}

window['primitives_uint4_method___operator_minus_uint4_uint'] = primitives_uint4_method___operator_minus_uint4_uint;
function primitives_int3_method___cast_from_vec_int3(other) {
return [(other[0] & 0xffffffff), (other[1] & 0xffffffff), (other[2] & 0xffffffff)];
}

window['primitives_int3_method___cast_from_vec_int3'] = primitives_int3_method___cast_from_vec_int3;
function primitives_short_method___operator_equals_short_short(__this, other) {
return __this === other;
}

window['primitives_short_method___operator_equals_short_short'] = primitives_short_method___operator_equals_short_short;
function primitives_int4_method___operator_divide_int4_int4(__this, other) {
return [(__this[0] / other[0]) & 0xffffffff, (__this[1] / other[1]) & 0xffffffff, (__this[2] / other[2]) & 0xffffffff, (__this[3] / other[3]) & 0xffffffff]
}

window['primitives_int4_method___operator_divide_int4_int4'] = primitives_int4_method___operator_divide_int4_int4;
function primitives_short_method___postfix_operator_join_short(__this) {
return (__this++);
}

window['primitives_short_method___postfix_operator_join_short'] = primitives_short_method___postfix_operator_join_short;
function primitives_half3_method___operator_modulo_half3_half(__this, other) {
return [(__this[0] % other), (__this[1] % other), (__this[2] % other)]
}

window['primitives_half3_method___operator_modulo_half3_half'] = primitives_half3_method___operator_modulo_half3_half;
/* cpu_only can except */function primitives_draw_shader(__stack_info, shade) {
try {
__shadeup_dispatch_draw(shade)} catch (e) {
  throw __shadeup_error(e, __stack_info);
}

}

window['primitives_draw_shader'] = primitives_draw_shader;
function primitives_dist_uint2_uint2(a, b) {
return dist(a, b);
}

window['primitives_dist_uint2_uint2'] = primitives_dist_uint2_uint2;
function primitives_int4_method___operator_plus_int4_int4(__this, other) {
return [(__this[0] + other[0]) & 0xffffffff, (__this[1] + other[1]) & 0xffffffff, (__this[2] + other[2]) & 0xffffffff, (__this[3] + other[3]) & 0xffffffff]
}

window['primitives_int4_method___operator_plus_int4_int4'] = primitives_int4_method___operator_plus_int4_int4;
function primitives_short3_method___operator_multiply_short3_short3(__this, other) {
return [(__this[0] * other[0]) & 0xffff, (__this[1] * other[1]) & 0xffff, (__this[2] * other[2]) & 0xffff]
}

window['primitives_short3_method___operator_multiply_short3_short3'] = primitives_short3_method___operator_multiply_short3_short3;
function primitives_uint4_method___construct_uint_uint_uint_uint(x, y, z, w) {
return [x, y, z, w];
}

window['primitives_uint4_method___construct_uint_uint_uint_uint'] = primitives_uint4_method___construct_uint_uint_uint_uint;
function primitives_double2_method___is_vec_2_double2_double2(__this, other) {

}

window['primitives_double2_method___is_vec_2_double2_double2'] = primitives_double2_method___is_vec_2_double2_double2;
function primitives_double_method___operator_plus_double_double(__this, other) {
return (__this + other);
}

window['primitives_double_method___operator_plus_double_double'] = primitives_double_method___operator_plus_double_double;
function primitives_uint4_method___operator_multiply_uint4_uint(__this, other) {
return [(__this[0] * other)>>>0 & 0xffffffff, (__this[1] * other)>>>0 & 0xffffffff, (__this[2] * other)>>>0 & 0xffffffff, (__this[3] * other)>>>0 & 0xffffffff]
}

window['primitives_uint4_method___operator_multiply_uint4_uint'] = primitives_uint4_method___operator_multiply_uint4_uint;
function primitives_half2_method___prefix_operator_minus_half2(__this) {
return [(-__this[0]), (-__this[1])]
}

window['primitives_half2_method___prefix_operator_minus_half2'] = primitives_half2_method___prefix_operator_minus_half2;
function primitives_uint3_method___construct_uint_uint_uint(x, y, z) {
return [x, y, z];
}

window['primitives_uint3_method___construct_uint_uint_uint'] = primitives_uint3_method___construct_uint_uint_uint;
function primitives_short_method___operator_greater_than_short_short(__this, other) {
return __this > other;
}

window['primitives_short_method___operator_greater_than_short_short'] = primitives_short_method___operator_greater_than_short_short;
function primitives_int3_method___make_vec(x, y, z) {
return [x, y, z];
}

window['primitives_int3_method___make_vec'] = primitives_int3_method___make_vec;
function primitives_uint4_method___operator_plus_uint4_uint(__this, other) {
return [(__this[0] + other)>>>0 & 0xffffffff, (__this[1] + other)>>>0 & 0xffffffff, (__this[2] + other)>>>0 & 0xffffffff, (__this[3] + other)>>>0 & 0xffffffff]
}

window['primitives_uint4_method___operator_plus_uint4_uint'] = primitives_uint4_method___operator_plus_uint4_uint;
function primitives_double2_method___operator_plus_double2_double2(__this, other) {
return [(__this[0] + other[0]), (__this[1] + other[1])]
}

window['primitives_double2_method___operator_plus_double2_double2'] = primitives_double2_method___operator_plus_double2_double2;
function primitives_float2_method___operator_modulo_float2_float(__this, other) {
return [(__this[0] % other), (__this[1] % other)]
}

window['primitives_float2_method___operator_modulo_float2_float'] = primitives_float2_method___operator_modulo_float2_float;
function primitives_double_method___postfix_operator_join_double(__this) {
return (__this++);
}

window['primitives_double_method___postfix_operator_join_double'] = primitives_double_method___postfix_operator_join_double;
function primitives_half3_method___operator_minus_half3_half(__this, other) {
return [(__this[0] - other), (__this[1] - other), (__this[2] - other)]
}

window['primitives_half3_method___operator_minus_half3_half'] = primitives_half3_method___operator_minus_half3_half;
function primitives_short_method___operator_multiply_short_short(__this, other) {
return (__this * other) & 0xffff;
}

window['primitives_short_method___operator_multiply_short_short'] = primitives_short_method___operator_multiply_short_short;
function primitives_float3_method___operator_minus_float3_float3(__this, other) {
return [(__this[0] - other[0]), (__this[1] - other[1]), (__this[2] - other[2])]
}

window['primitives_float3_method___operator_minus_float3_float3'] = primitives_float3_method___operator_minus_float3_float3;
function primitives_half_method___operator_double_multiply_half_half(__this, other) {
return (__this ** other);
}

window['primitives_half_method___operator_double_multiply_half_half'] = primitives_half_method___operator_double_multiply_half_half;
function primitives_dist_int4_int4(a, b) {
return Math.sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y) + (a.z - b.z) * (a.z - b.z) + (a.w - b.w) * (a.w - b.w));
}

window['primitives_dist_int4_int4'] = primitives_dist_int4_int4;
function primitives_half3_method___operator_modulo_half3_half3(__this, other) {
return [(__this[0] % other[0]), (__this[1] % other[1]), (__this[2] % other[2])]
}

window['primitives_half3_method___operator_modulo_half3_half3'] = primitives_half3_method___operator_modulo_half3_half3;
function primitives_int2_method___prefix_operator_minus_int2(__this) {
return [(-__this[0]) & 0xffffffff, (-__this[1]) & 0xffffffff]
}

window['primitives_int2_method___prefix_operator_minus_int2'] = primitives_int2_method___prefix_operator_minus_int2;
function primitives_dist_float3_float3(a, b) {
return Math.sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y) + (a.z - b.z) * (a.z - b.z));
}

window['primitives_dist_float3_float3'] = primitives_dist_float3_float3;
function primitives_float3_method___operator_multiply_float3_float(__this, other) {
return [(__this[0] * other), (__this[1] * other), (__this[2] * other)]
}

window['primitives_float3_method___operator_multiply_float3_float'] = primitives_float3_method___operator_multiply_float3_float;
function primitives_int2_method___operator_divide_int2_int2(__this, other) {
return [(__this[0] / other[0]) & 0xffffffff, (__this[1] / other[1]) & 0xffffffff]
}

window['primitives_int2_method___operator_divide_int2_int2'] = primitives_int2_method___operator_divide_int2_int2;
function primitives_half_method___operator_modulo_half_half(__this, other) {
return (__this % other);
}

window['primitives_half_method___operator_modulo_half_half'] = primitives_half_method___operator_modulo_half_half;
function primitives_dist_short_short(a, b) {
return Math.abs(a - b);
}

window['primitives_dist_short_short'] = primitives_dist_short_short;
function primitives_uint4_method___operator_minus_uint4_uint4(__this, other) {
return [(__this[0] - other[0])>>>0 & 0xffffffff, (__this[1] - other[1])>>>0 & 0xffffffff, (__this[2] - other[2])>>>0 & 0xffffffff, (__this[3] - other[3])>>>0 & 0xffffffff]
}

window['primitives_uint4_method___operator_minus_uint4_uint4'] = primitives_uint4_method___operator_minus_uint4_uint4;
function primitives_dist_float_float(a, b) {
return Math.abs(a - b);
}

window['primitives_dist_float_float'] = primitives_dist_float_float;
function primitives_int3_method___operator_double_multiply_int3_int3(__this, other) {
return [(__this[0] ** other[0]) & 0xffffffff, (__this[1] ** other[1]) & 0xffffffff, (__this[2] ** other[2]) & 0xffffffff]
}

window['primitives_int3_method___operator_double_multiply_int3_int3'] = primitives_int3_method___operator_double_multiply_int3_int3;
function primitives_uint2_method___is_vec_2_uint2_uint2(__this, other) {

}

window['primitives_uint2_method___is_vec_2_uint2_uint2'] = primitives_uint2_method___is_vec_2_uint2_uint2;
function primitives_double2_method___operator_multiply_double2_double2(__this, other) {
return [(__this[0] * other[0]), (__this[1] * other[1])]
}

window['primitives_double2_method___operator_multiply_double2_double2'] = primitives_double2_method___operator_multiply_double2_double2;
function primitives_double_method___operator_not_equals_double_double(__this, other) {
return __this !== other;
}

window['primitives_double_method___operator_not_equals_double_double'] = primitives_double_method___operator_not_equals_double_double;
function primitives_short_method___prefix_operator_minus_minus_short(__this) {
return (--__this);
}

window['primitives_short_method___prefix_operator_minus_minus_short'] = primitives_short_method___prefix_operator_minus_minus_short;
function primitives_double4_method___operator_minus_double4_double(__this, other) {
return [(__this[0] - other), (__this[1] - other), (__this[2] - other), (__this[3] - other)]
}

window['primitives_double4_method___operator_minus_double4_double'] = primitives_double4_method___operator_minus_double4_double;
function primitives_float4_method___operator_modulo_float4_float4(__this, other) {
return [(__this[0] % other[0]), (__this[1] % other[1]), (__this[2] % other[2]), (__this[3] % other[3])]
}

window['primitives_float4_method___operator_modulo_float4_float4'] = primitives_float4_method___operator_modulo_float4_float4;
function primitives_uint_method___operator_plus_uint_uint(__this, other) {
return (__this + other)>>>0 & 0xffffffff;
}

window['primitives_uint_method___operator_plus_uint_uint'] = primitives_uint_method___operator_plus_uint_uint;
function primitives_uint_method___operator_modulo_uint_uint(__this, other) {
return (__this % other)>>>0 & 0xffffffff;
}

window['primitives_uint_method___operator_modulo_uint_uint'] = primitives_uint_method___operator_modulo_uint_uint;
function primitives_uint_method___operator_greater_than_uint_uint(__this, other) {
return __this > other;
}

window['primitives_uint_method___operator_greater_than_uint_uint'] = primitives_uint_method___operator_greater_than_uint_uint;
function primitives_int4_method___operator_multiply_int4_int(__this, other) {
return [(__this[0] * other) & 0xffffffff, (__this[1] * other) & 0xffffffff, (__this[2] * other) & 0xffffffff, (__this[3] * other) & 0xffffffff]
}

window['primitives_int4_method___operator_multiply_int4_int'] = primitives_int4_method___operator_multiply_int4_int;
function primitives_float3_method___operator_modulo_float3_float(__this, other) {
return [(__this[0] % other), (__this[1] % other), (__this[2] % other)]
}

window['primitives_float3_method___operator_modulo_float3_float'] = primitives_float3_method___operator_modulo_float3_float;
function primitives_array_method_len_array___any___(__this) {
return __this.length
}

window['primitives_array_method_len_array___any___'] = primitives_array_method_len_array___any___;
function primitives_int4_method___operator_minus_int4_int4(__this, other) {
return [(__this[0] - other[0]) & 0xffffffff, (__this[1] - other[1]) & 0xffffffff, (__this[2] - other[2]) & 0xffffffff, (__this[3] - other[3]) & 0xffffffff]
}

window['primitives_int4_method___operator_minus_int4_int4'] = primitives_int4_method___operator_minus_int4_int4;
function primitives_int4_method___operator_multiply_int4_int4(__this, other) {
return [(__this[0] * other[0]) & 0xffffffff, (__this[1] * other[1]) & 0xffffffff, (__this[2] * other[2]) & 0xffffffff, (__this[3] * other[3]) & 0xffffffff]
}

window['primitives_int4_method___operator_multiply_int4_int4'] = primitives_int4_method___operator_multiply_int4_int4;
function primitives_uint_method___operator_not_equals_uint_uint(__this, other) {
return __this !== other;
}

window['primitives_uint_method___operator_not_equals_uint_uint'] = primitives_uint_method___operator_not_equals_uint_uint;
function primitives_short_method___postfix_operator_minus_minus_short(__this) {
return (__this--);
}

window['primitives_short_method___postfix_operator_minus_minus_short'] = primitives_short_method___postfix_operator_minus_minus_short;
function primitives_float3_method___cast_from_vec_float3(other) {
return [(other[0]), (other[1]), (other[2])];
}

window['primitives_float3_method___cast_from_vec_float3'] = primitives_float3_method___cast_from_vec_float3;
function primitives_uint2_method___operator_plus_uint2_uint2(__this, other) {
return [(__this[0] + other[0])>>>0 & 0xffffffff, (__this[1] + other[1])>>>0 & 0xffffffff]
}

window['primitives_uint2_method___operator_plus_uint2_uint2'] = primitives_uint2_method___operator_plus_uint2_uint2;
function primitives_uint_method___operator_hat_uint_uint(__this, other) {
return (__this ^ other)>>>0 & 0xffffffff;
}

window['primitives_uint_method___operator_hat_uint_uint'] = primitives_uint_method___operator_hat_uint_uint;
function primitives_short3_method___operator_modulo_short3_short3(__this, other) {
return [(__this[0] % other[0]) & 0xffff, (__this[1] % other[1]) & 0xffff, (__this[2] % other[2]) & 0xffff]
}

window['primitives_short3_method___operator_modulo_short3_short3'] = primitives_short3_method___operator_modulo_short3_short3;
function primitives_float_method___prefix_operator_minus_float(__this) {
return (-__this);
}

window['primitives_float_method___prefix_operator_minus_float'] = primitives_float_method___prefix_operator_minus_float;
function primitives_half4_method___operator_cross_half4_half4(__this, other) {
return [(__this[0] * other[0]), (__this[1] * other[1]), (__this[2] * other[2]), (__this[3] * other[3])]
}

window['primitives_half4_method___operator_cross_half4_half4'] = primitives_half4_method___operator_cross_half4_half4;
function primitives_short_method___is_scalar_short_short(__this, other) {

}

window['primitives_short_method___is_scalar_short_short'] = primitives_short_method___is_scalar_short_short;
function primitives_double2_method___operator_minus_double2_double(__this, other) {
return [(__this[0] - other), (__this[1] - other)]
}

window['primitives_double2_method___operator_minus_double2_double'] = primitives_double2_method___operator_minus_double2_double;
function primitives_int4_method___construct_int_int_int_int(x, y, z, w) {
return [x, y, z, w];
}

window['primitives_int4_method___construct_int_int_int_int'] = primitives_int4_method___construct_int_int_int_int;
function primitives_half4_method___construct_half_half_half_half(x, y, z, w) {
return [x, y, z, w];
}

window['primitives_half4_method___construct_half_half_half_half'] = primitives_half4_method___construct_half_half_half_half;
function primitives_double_method___operator_multiply_double_double(__this, other) {
return (__this * other);
}

window['primitives_double_method___operator_multiply_double_double'] = primitives_double_method___operator_multiply_double_double;
function primitives_uint4_method___operator_modulo_uint4_uint(__this, other) {
return [(__this[0] % other)>>>0 & 0xffffffff, (__this[1] % other)>>>0 & 0xffffffff, (__this[2] % other)>>>0 & 0xffffffff, (__this[3] % other)>>>0 & 0xffffffff]
}

window['primitives_uint4_method___operator_modulo_uint4_uint'] = primitives_uint4_method___operator_modulo_uint4_uint;
function primitives_double2_method___prefix_operator_minus_double2(__this) {
return [(-__this[0]), (-__this[1])]
}

window['primitives_double2_method___prefix_operator_minus_double2'] = primitives_double2_method___prefix_operator_minus_double2;
function primitives_array_method_push_array___any____T0(__this, value) {
__this.push(value);
}

window['primitives_array_method_push_array___any____T0'] = primitives_array_method_push_array___any____T0;
function primitives_half3_method___operator_plus_half3_half(__this, other) {
return [(__this[0] + other), (__this[1] + other), (__this[2] + other)]
}

window['primitives_half3_method___operator_plus_half3_half'] = primitives_half3_method___operator_plus_half3_half;
function primitives_short3_method___prefix_operator_minus_short3(__this) {
return [(-__this[0]) & 0xffff, (-__this[1]) & 0xffff, (-__this[2]) & 0xffff]
}

window['primitives_short3_method___prefix_operator_minus_short3'] = primitives_short3_method___prefix_operator_minus_short3;
function primitives_short2_method___operator_minus_short2_short2(__this, other) {
return [(__this[0] - other[0]) & 0xffff, (__this[1] - other[1]) & 0xffff]
}

window['primitives_short2_method___operator_minus_short2_short2'] = primitives_short2_method___operator_minus_short2_short2;
function primitives_uint_method___postfix_operator_minus_minus_uint(__this) {
return (__this--);
}

window['primitives_uint_method___postfix_operator_minus_minus_uint'] = primitives_uint_method___postfix_operator_minus_minus_uint;
function primitives_float4_method___is_vec_4_float4_float4(__this, other) {

}

window['primitives_float4_method___is_vec_4_float4_float4'] = primitives_float4_method___is_vec_4_float4_float4;
function primitives_half2_method___operator_divide_half2_half2(__this, other) {
return [(__this[0] / other[0]), (__this[1] / other[1])]
}

window['primitives_half2_method___operator_divide_half2_half2'] = primitives_half2_method___operator_divide_half2_half2;
function primitives_half3_method___prefix_operator_minus_half3(__this) {
return [(-__this[0]), (-__this[1]), (-__this[2])]
}

window['primitives_half3_method___prefix_operator_minus_half3'] = primitives_half3_method___prefix_operator_minus_half3;
function primitives_double2_method___cast_from_vec_double2(other) {
return [(other[0]), (other[1])];
}

window['primitives_double2_method___cast_from_vec_double2'] = primitives_double2_method___cast_from_vec_double2;
function primitives_short_method___operator_double_multiply_short_short(__this, other) {
return (__this ** other) & 0xffff;
}

window['primitives_short_method___operator_double_multiply_short_short'] = primitives_short_method___operator_double_multiply_short_short;
function primitives_float_method___operator_modulo_float_float(__this, other) {
return (__this % other);
}

window['primitives_float_method___operator_modulo_float_float'] = primitives_float_method___operator_modulo_float_float;
function primitives_float3_method___operator_cross_float3_float3(__this, other) {
return [(__this[0] * other[0]), (__this[1] * other[1]), (__this[2] * other[2])]
}

window['primitives_float3_method___operator_cross_float3_float3'] = primitives_float3_method___operator_cross_float3_float3;
function primitives_dist_half2_half2(a, b) {
return dist(a, b);
}

window['primitives_dist_half2_half2'] = primitives_dist_half2_half2;
function primitives_short4_method___operator_modulo_short4_short4(__this, other) {
return [(__this[0] % other[0]) & 0xffff, (__this[1] % other[1]) & 0xffff, (__this[2] % other[2]) & 0xffff, (__this[3] % other[3]) & 0xffff]
}

window['primitives_short4_method___operator_modulo_short4_short4'] = primitives_short4_method___operator_modulo_short4_short4;
function primitives_half_method___operator_less_than_or_equals_half_half(__this, other) {
return __this <= other;
}

window['primitives_half_method___operator_less_than_or_equals_half_half'] = primitives_half_method___operator_less_than_or_equals_half_half;
function primitives_float_method___operator_not_equals_float_float(__this, other) {
return __this !== other;
}

window['primitives_float_method___operator_not_equals_float_float'] = primitives_float_method___operator_not_equals_float_float;
function primitives_double4_method___operator_multiply_double4_double4(__this, other) {
return [(__this[0] * other[0]), (__this[1] * other[1]), (__this[2] * other[2]), (__this[3] * other[3])]
}

window['primitives_double4_method___operator_multiply_double4_double4'] = primitives_double4_method___operator_multiply_double4_double4;
function primitives_int3_method___operator_cross_int3_int3(__this, other) {
return [(__this[0] * other[0]) & 0xffffffff, (__this[1] * other[1]) & 0xffffffff, (__this[2] * other[2]) & 0xffffffff]
}

window['primitives_int3_method___operator_cross_int3_int3'] = primitives_int3_method___operator_cross_int3_int3;
function primitives_float_method___operator_multiply_float_float(__this, other) {
return (__this * other);
}

window['primitives_float_method___operator_multiply_float_float'] = primitives_float_method___operator_multiply_float_float;
function primitives_double2_method___operator_multiply_double2_double(__this, other) {
return [(__this[0] * other), (__this[1] * other)]
}

window['primitives_double2_method___operator_multiply_double2_double'] = primitives_double2_method___operator_multiply_double2_double;
function primitives_uint4_method___operator_multiply_uint4_uint4(__this, other) {
return [(__this[0] * other[0])>>>0 & 0xffffffff, (__this[1] * other[1])>>>0 & 0xffffffff, (__this[2] * other[2])>>>0 & 0xffffffff, (__this[3] * other[3])>>>0 & 0xffffffff]
}

window['primitives_uint4_method___operator_multiply_uint4_uint4'] = primitives_uint4_method___operator_multiply_uint4_uint4;
function primitives_float2_method___operator_minus_float2_float(__this, other) {
return [(__this[0] - other), (__this[1] - other)]
}

window['primitives_float2_method___operator_minus_float2_float'] = primitives_float2_method___operator_minus_float2_float;
function primitives_float3_method___operator_plus_float3_float3(__this, other) {
return [(__this[0] + other[0]), (__this[1] + other[1]), (__this[2] + other[2])]
}

window['primitives_float3_method___operator_plus_float3_float3'] = primitives_float3_method___operator_plus_float3_float3;
function primitives_uint3_method___operator_plus_uint3_uint3(__this, other) {
return [(__this[0] + other[0])>>>0 & 0xffffffff, (__this[1] + other[1])>>>0 & 0xffffffff, (__this[2] + other[2])>>>0 & 0xffffffff]
}

window['primitives_uint3_method___operator_plus_uint3_uint3'] = primitives_uint3_method___operator_plus_uint3_uint3;
function primitives_short_method___operator_double_right_short_short(__this, other) {
return (__this >> other) & 0xffff;
}

window['primitives_short_method___operator_double_right_short_short'] = primitives_short_method___operator_double_right_short_short;
function primitives_uint2_method___construct_uint_uint(x, y) {
return [x, y];
}

window['primitives_uint2_method___construct_uint_uint'] = primitives_uint2_method___construct_uint_uint;
function primitives_uint3_method___operator_cross_uint3_uint3(__this, other) {
return [(__this[0] * other[0])>>>0 & 0xffffffff, (__this[1] * other[1])>>>0 & 0xffffffff, (__this[2] * other[2])>>>0 & 0xffffffff]
}

window['primitives_uint3_method___operator_cross_uint3_uint3'] = primitives_uint3_method___operator_cross_uint3_uint3;
function primitives_double_method___prefix_operator_minus_minus_double(__this) {
return (--__this);
}

window['primitives_double_method___prefix_operator_minus_minus_double'] = primitives_double_method___prefix_operator_minus_minus_double;
function primitives_double_method___is_scalar_double_double(__this, other) {

}

window['primitives_double_method___is_scalar_double_double'] = primitives_double_method___is_scalar_double_double;
function primitives_float_method___cast_from_scalar_float(other) {
return other;
}

window['primitives_float_method___cast_from_scalar_float'] = primitives_float_method___cast_from_scalar_float;
function primitives_dist_uint3_uint3(a, b) {
return Math.sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y) + (a.z - b.z) * (a.z - b.z));
}

window['primitives_dist_uint3_uint3'] = primitives_dist_uint3_uint3;
function primitives_short2_method___operator_minus_short2_short(__this, other) {
return [(__this[0] - other) & 0xffff, (__this[1] - other) & 0xffff]
}

window['primitives_short2_method___operator_minus_short2_short'] = primitives_short2_method___operator_minus_short2_short;
function primitives_double4_method___operator_double_multiply_double4_double4(__this, other) {
return [(__this[0] ** other[0]), (__this[1] ** other[1]), (__this[2] ** other[2]), (__this[3] ** other[3])]
}

window['primitives_double4_method___operator_double_multiply_double4_double4'] = primitives_double4_method___operator_double_multiply_double4_double4;
function primitives_int3_method___operator_divide_int3_int3(__this, other) {
return [(__this[0] / other[0]) & 0xffffffff, (__this[1] / other[1]) & 0xffffffff, (__this[2] / other[2]) & 0xffffffff]
}

window['primitives_int3_method___operator_divide_int3_int3'] = primitives_int3_method___operator_divide_int3_int3;
function primitives_uint_method___prefix_operator_minus_minus_uint(__this) {
return (--__this);
}

window['primitives_uint_method___prefix_operator_minus_minus_uint'] = primitives_uint_method___prefix_operator_minus_minus_uint;
function primitives_uint3_method___operator_multiply_uint3_uint3(__this, other) {
return [(__this[0] * other[0])>>>0 & 0xffffffff, (__this[1] * other[1])>>>0 & 0xffffffff, (__this[2] * other[2])>>>0 & 0xffffffff]
}

window['primitives_uint3_method___operator_multiply_uint3_uint3'] = primitives_uint3_method___operator_multiply_uint3_uint3;
function primitives_half3_method___construct_half_half_half(x, y, z) {
return [x, y, z];
}

window['primitives_half3_method___construct_half_half_half'] = primitives_half3_method___construct_half_half_half;
function primitives_dist_short2_short2(a, b) {
return dist(a, b);
}

window['primitives_dist_short2_short2'] = primitives_dist_short2_short2;
function primitives_int3_method___operator_minus_int3_int3(__this, other) {
return [(__this[0] - other[0]) & 0xffffffff, (__this[1] - other[1]) & 0xffffffff, (__this[2] - other[2]) & 0xffffffff]
}

window['primitives_int3_method___operator_minus_int3_int3'] = primitives_int3_method___operator_minus_int3_int3;
function primitives_float_method___operator_less_than_or_equals_float_float(__this, other) {
return __this <= other;
}

window['primitives_float_method___operator_less_than_or_equals_float_float'] = primitives_float_method___operator_less_than_or_equals_float_float;
function primitives_double2_method___operator_modulo_double2_double2(__this, other) {
return [(__this[0] % other[0]), (__this[1] % other[1])]
}

window['primitives_double2_method___operator_modulo_double2_double2'] = primitives_double2_method___operator_modulo_double2_double2;
function primitives_double_method___postfix_operator_minus_minus_double(__this) {
return (__this--);
}

window['primitives_double_method___postfix_operator_minus_minus_double'] = primitives_double_method___postfix_operator_minus_minus_double;
function primitives_double3_method___operator_double_multiply_double3_double3(__this, other) {
return [(__this[0] ** other[0]), (__this[1] ** other[1]), (__this[2] ** other[2])]
}

window['primitives_double3_method___operator_double_multiply_double3_double3'] = primitives_double3_method___operator_double_multiply_double3_double3;
function primitives_double_method___operator_modulo_double_double(__this, other) {
return (__this % other);
}

window['primitives_double_method___operator_modulo_double_double'] = primitives_double_method___operator_modulo_double_double;
function primitives_float3_method___make_vec(x, y, z) {
return [x, y, z];
}

window['primitives_float3_method___make_vec'] = primitives_float3_method___make_vec;
function primitives_double3_method___operator_minus_double3_double3(__this, other) {
return [(__this[0] - other[0]), (__this[1] - other[1]), (__this[2] - other[2])]
}

window['primitives_double3_method___operator_minus_double3_double3'] = primitives_double3_method___operator_minus_double3_double3;
function primitives_half3_method___make_vec(x, y, z) {
return [x, y, z];
}

window['primitives_half3_method___make_vec'] = primitives_half3_method___make_vec;
function primitives_float4_method___operator_multiply_float4_float4(__this, other) {
return [(__this[0] * other[0]), (__this[1] * other[1]), (__this[2] * other[2]), (__this[3] * other[3])]
}

window['primitives_float4_method___operator_multiply_float4_float4'] = primitives_float4_method___operator_multiply_float4_float4;
function primitives_short2_method___operator_divide_short2_short(__this, other) {
return [(__this[0] / other) & 0xffff, (__this[1] / other) & 0xffff]
}

window['primitives_short2_method___operator_divide_short2_short'] = primitives_short2_method___operator_divide_short2_short;
function primitives_double3_method___operator_modulo_double3_double(__this, other) {
return [(__this[0] % other), (__this[1] % other), (__this[2] % other)]
}

window['primitives_double3_method___operator_modulo_double3_double'] = primitives_double3_method___operator_modulo_double3_double;
function primitives_short_method___operator_minus_short_short(__this, other) {
return (__this - other) & 0xffff;
}

window['primitives_short_method___operator_minus_short_short'] = primitives_short_method___operator_minus_short_short;
function primitives_double3_method___operator_divide_double3_double(__this, other) {
return [(__this[0] / other), (__this[1] / other), (__this[2] / other)]
}

window['primitives_double3_method___operator_divide_double3_double'] = primitives_double3_method___operator_divide_double3_double;
function primitives_float2_method___operator_multiply_float2_float(__this, other) {
return [(__this[0] * other), (__this[1] * other)]
}

window['primitives_float2_method___operator_multiply_float2_float'] = primitives_float2_method___operator_multiply_float2_float;
function primitives_short_method___operator_less_than_or_equals_short_short(__this, other) {
return __this <= other;
}

window['primitives_short_method___operator_less_than_or_equals_short_short'] = primitives_short_method___operator_less_than_or_equals_short_short;
function primitives_dist_double_double(a, b) {
return Math.abs(a - b);
}

window['primitives_dist_double_double'] = primitives_dist_double_double;
function primitives_half4_method___operator_double_multiply_half4_half4(__this, other) {
return [(__this[0] ** other[0]), (__this[1] ** other[1]), (__this[2] ** other[2]), (__this[3] ** other[3])]
}

window['primitives_half4_method___operator_double_multiply_half4_half4'] = primitives_half4_method___operator_double_multiply_half4_half4;
function primitives_double3_method___make_vec(x, y, z) {
return [x, y, z];
}

window['primitives_double3_method___make_vec'] = primitives_double3_method___make_vec;
function primitives_uint_method___prefix_operator_join_uint(__this) {
return (++__this);
}

window['primitives_uint_method___prefix_operator_join_uint'] = primitives_uint_method___prefix_operator_join_uint;
function primitives_uint3_method___operator_divide_uint3_uint(__this, other) {
return [(__this[0] / other)>>>0 & 0xffffffff, (__this[1] / other)>>>0 & 0xffffffff, (__this[2] / other)>>>0 & 0xffffffff]
}

window['primitives_uint3_method___operator_divide_uint3_uint'] = primitives_uint3_method___operator_divide_uint3_uint;
function primitives_uint2_method___operator_multiply_uint2_uint(__this, other) {
return [(__this[0] * other)>>>0 & 0xffffffff, (__this[1] * other)>>>0 & 0xffffffff]
}

window['primitives_uint2_method___operator_multiply_uint2_uint'] = primitives_uint2_method___operator_multiply_uint2_uint;
function primitives_short4_method___is_vec_4_short4_short4(__this, other) {

}

window['primitives_short4_method___is_vec_4_short4_short4'] = primitives_short4_method___is_vec_4_short4_short4;
function primitives_uint2_method___operator_cross_uint2_uint2(__this, other) {
return [(__this[0] * other[0])>>>0 & 0xffffffff, (__this[1] * other[1])>>>0 & 0xffffffff]
}

window['primitives_uint2_method___operator_cross_uint2_uint2'] = primitives_uint2_method___operator_cross_uint2_uint2;
function primitives_short_method___operator_not_equals_short_short(__this, other) {
return __this !== other;
}

window['primitives_short_method___operator_not_equals_short_short'] = primitives_short_method___operator_not_equals_short_short;
function primitives_float3_method___operator_divide_float3_float(__this, other) {
return [(__this[0] / other), (__this[1] / other), (__this[2] / other)]
}

window['primitives_float3_method___operator_divide_float3_float'] = primitives_float3_method___operator_divide_float3_float;
function primitives_dist_float4_float4(a, b) {
return Math.sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y) + (a.z - b.z) * (a.z - b.z) + (a.w - b.w) * (a.w - b.w));
}

window['primitives_dist_float4_float4'] = primitives_dist_float4_float4;
function primitives_byte_method___operator_not_equals_byte_byte(__this, other) {
return __this !== other;
}

window['primitives_byte_method___operator_not_equals_byte_byte'] = primitives_byte_method___operator_not_equals_byte_byte;
function primitives_uint3_method___make_vec(x, y, z) {
return [x, y, z];
}

window['primitives_uint3_method___make_vec'] = primitives_uint3_method___make_vec;
function primitives_uint_method___operator_less_than_uint_uint(__this, other) {
return __this < other;
}

window['primitives_uint_method___operator_less_than_uint_uint'] = primitives_uint_method___operator_less_than_uint_uint;
function primitives_double3_method___operator_double_multiply_double3_double(__this, other) {
return [(__this[0] ** other), (__this[1] ** other), (__this[2] ** other)]
}

window['primitives_double3_method___operator_double_multiply_double3_double'] = primitives_double3_method___operator_double_multiply_double3_double;
function primitives_half4_method___operator_minus_half4_half(__this, other) {
return [(__this[0] - other), (__this[1] - other), (__this[2] - other), (__this[3] - other)]
}

window['primitives_half4_method___operator_minus_half4_half'] = primitives_half4_method___operator_minus_half4_half;
function primitives_uint4_method___operator_modulo_uint4_uint4(__this, other) {
return [(__this[0] % other[0])>>>0 & 0xffffffff, (__this[1] % other[1])>>>0 & 0xffffffff, (__this[2] % other[2])>>>0 & 0xffffffff, (__this[3] % other[3])>>>0 & 0xffffffff]
}

window['primitives_uint4_method___operator_modulo_uint4_uint4'] = primitives_uint4_method___operator_modulo_uint4_uint4;
function primitives_int2_method___operator_plus_int2_int2(__this, other) {
return [(__this[0] + other[0]) & 0xffffffff, (__this[1] + other[1]) & 0xffffffff]
}

window['primitives_int2_method___operator_plus_int2_int2'] = primitives_int2_method___operator_plus_int2_int2;
function primitives_int_method___operator_divide_int_int(__this, other) {
return (__this / other) & 0xffffffff;
}

window['primitives_int_method___operator_divide_int_int'] = primitives_int_method___operator_divide_int_int;
function primitives_double4_method___operator_modulo_double4_double(__this, other) {
return [(__this[0] % other), (__this[1] % other), (__this[2] % other), (__this[3] % other)]
}

window['primitives_double4_method___operator_modulo_double4_double'] = primitives_double4_method___operator_modulo_double4_double;
function primitives_float2_method___make_vec(x, y) {
return [x, y];
}

window['primitives_float2_method___make_vec'] = primitives_float2_method___make_vec;
function primitives_double4_method___operator_double_multiply_double4_double(__this, other) {
return [(__this[0] ** other), (__this[1] ** other), (__this[2] ** other), (__this[3] ** other)]
}

window['primitives_double4_method___operator_double_multiply_double4_double'] = primitives_double4_method___operator_double_multiply_double4_double;
function primitives_half4_method___operator_divide_half4_half4(__this, other) {
return [(__this[0] / other[0]), (__this[1] / other[1]), (__this[2] / other[2]), (__this[3] / other[3])]
}

window['primitives_half4_method___operator_divide_half4_half4'] = primitives_half4_method___operator_divide_half4_half4;
function primitives_float4_method___construct_float_float_float_float(x, y, z, w) {
return [x, y, z, w];
}

window['primitives_float4_method___construct_float_float_float_float'] = primitives_float4_method___construct_float_float_float_float;
function primitives_uint4_method___prefix_operator_minus_uint4(__this) {
return [(-__this[0])>>>0 & 0xffffffff, (-__this[1])>>>0 & 0xffffffff, (-__this[2])>>>0 & 0xffffffff, (-__this[3])>>>0 & 0xffffffff]
}

window['primitives_uint4_method___prefix_operator_minus_uint4'] = primitives_uint4_method___prefix_operator_minus_uint4;
function primitives_uint4_method___operator_double_multiply_uint4_uint(__this, other) {
return [(__this[0] ** other)>>>0 & 0xffffffff, (__this[1] ** other)>>>0 & 0xffffffff, (__this[2] ** other)>>>0 & 0xffffffff, (__this[3] ** other)>>>0 & 0xffffffff]
}

window['primitives_uint4_method___operator_double_multiply_uint4_uint'] = primitives_uint4_method___operator_double_multiply_uint4_uint;
function primitives_double_method___operator_greater_than_or_equals_double_double(__this, other) {
return __this >= other;
}

window['primitives_double_method___operator_greater_than_or_equals_double_double'] = primitives_double_method___operator_greater_than_or_equals_double_double;
function primitives_uint3_method___operator_modulo_uint3_uint(__this, other) {
return [(__this[0] % other)>>>0 & 0xffffffff, (__this[1] % other)>>>0 & 0xffffffff, (__this[2] % other)>>>0 & 0xffffffff]
}

window['primitives_uint3_method___operator_modulo_uint3_uint'] = primitives_uint3_method___operator_modulo_uint3_uint;
function primitives_half2_method___operator_minus_half2_half(__this, other) {
return [(__this[0] - other), (__this[1] - other)]
}

window['primitives_half2_method___operator_minus_half2_half'] = primitives_half2_method___operator_minus_half2_half;
function primitives_float_method___operator_equals_float_float(__this, other) {
return __this === other;
}

window['primitives_float_method___operator_equals_float_float'] = primitives_float_method___operator_equals_float_float;
function primitives_short_method___operator_and_short_short(__this, other) {
return (__this & other) & 0xffff;
}

window['primitives_short_method___operator_and_short_short'] = primitives_short_method___operator_and_short_short;
function primitives_double2_method___operator_plus_double2_double(__this, other) {
return [(__this[0] + other), (__this[1] + other)]
}

window['primitives_double2_method___operator_plus_double2_double'] = primitives_double2_method___operator_plus_double2_double;
function primitives_float_method___is_scalar_float_float(__this, other) {

}

window['primitives_float_method___is_scalar_float_float'] = primitives_float_method___is_scalar_float_float;
function __make_struct_test__shadeup_Player(fields) {
return {name: fields.name, health: fields.health, };
}

window['__make_struct_test__shadeup_Player'] = __make_struct_test__shadeup_Player;
function primitives_half2_method___make_vec(x, y) {
return [x, y];
}

window['primitives_half2_method___make_vec'] = primitives_half2_method___make_vec;
function primitives_float3_method___operator_multiply_float3_float3(__this, other) {
return [(__this[0] * other[0]), (__this[1] * other[1]), (__this[2] * other[2])]
}

window['primitives_float3_method___operator_multiply_float3_float3'] = primitives_float3_method___operator_multiply_float3_float3;
function primitives_uint3_method___is_vec_3_uint3_uint3(__this, other) {

}

window['primitives_uint3_method___is_vec_3_uint3_uint3'] = primitives_uint3_method___is_vec_3_uint3_uint3;
function primitives_double4_method___operator_modulo_double4_double4(__this, other) {
return [(__this[0] % other[0]), (__this[1] % other[1]), (__this[2] % other[2]), (__this[3] % other[3])]
}

window['primitives_double4_method___operator_modulo_double4_double4'] = primitives_double4_method___operator_modulo_double4_double4;
function primitives_int3_method___operator_plus_int3_int3(__this, other) {
return [(__this[0] + other[0]) & 0xffffffff, (__this[1] + other[1]) & 0xffffffff, (__this[2] + other[2]) & 0xffffffff]
}

window['primitives_int3_method___operator_plus_int3_int3'] = primitives_int3_method___operator_plus_int3_int3;
function primitives_float4_method___make_vec(x, y, z, w) {
return [x, y, z, w];
}

window['primitives_float4_method___make_vec'] = primitives_float4_method___make_vec;
function primitives_int3_method___operator_minus_int3_int(__this, other) {
return [(__this[0] - other) & 0xffffffff, (__this[1] - other) & 0xffffffff, (__this[2] - other) & 0xffffffff]
}

window['primitives_int3_method___operator_minus_int3_int'] = primitives_int3_method___operator_minus_int3_int;
function primitives_short_method___operator_modulo_short_short(__this, other) {
return (__this % other) & 0xffff;
}

window['primitives_short_method___operator_modulo_short_short'] = primitives_short_method___operator_modulo_short_short;
function primitives_int_method___operator_modulo_int_int(__this, other) {
return (__this % other) & 0xffffffff;
}

window['primitives_int_method___operator_modulo_int_int'] = primitives_int_method___operator_modulo_int_int;
function primitives_uint2_method___operator_modulo_uint2_uint(__this, other) {
return [(__this[0] % other)>>>0 & 0xffffffff, (__this[1] % other)>>>0 & 0xffffffff]
}

window['primitives_uint2_method___operator_modulo_uint2_uint'] = primitives_uint2_method___operator_modulo_uint2_uint;
function primitives_double_method___operator_divide_double_double(__this, other) {
return (__this / other);
}

window['primitives_double_method___operator_divide_double_double'] = primitives_double_method___operator_divide_double_double;
function primitives_dist_half_half(a, b) {
return Math.abs(a - b);
}

window['primitives_dist_half_half'] = primitives_dist_half_half;
function primitives_uint_method___operator_double_right_uint_uint(__this, other) {
return (__this >> other)>>>0 & 0xffffffff;
}

window['primitives_uint_method___operator_double_right_uint_uint'] = primitives_uint_method___operator_double_right_uint_uint;
function primitives_int_method___prefix_operator_minus_minus_int(__this) {
return (--__this);
}

window['primitives_int_method___prefix_operator_minus_minus_int'] = primitives_int_method___prefix_operator_minus_minus_int;
function primitives_int3_method___operator_plus_int3_int(__this, other) {
return [(__this[0] + other) & 0xffffffff, (__this[1] + other) & 0xffffffff, (__this[2] + other) & 0xffffffff]
}

window['primitives_int3_method___operator_plus_int3_int'] = primitives_int3_method___operator_plus_int3_int;
function primitives_int_method___postfix_operator_minus_minus_int(__this) {
return (__this--);
}

window['primitives_int_method___postfix_operator_minus_minus_int'] = primitives_int_method___postfix_operator_minus_minus_int;
function primitives_half2_method___operator_plus_half2_half(__this, other) {
return [(__this[0] + other), (__this[1] + other)]
}

window['primitives_half2_method___operator_plus_half2_half'] = primitives_half2_method___operator_plus_half2_half;
function primitives_int4_method___cast_from_vec_int4(other) {
return [(other[0] & 0xffffffff), (other[1] & 0xffffffff), (other[2] & 0xffffffff), (other[3] & 0xffffffff)];
}

window['primitives_int4_method___cast_from_vec_int4'] = primitives_int4_method___cast_from_vec_int4;
function primitives_short_method___operator_less_than_short_short(__this, other) {
return __this < other;
}

window['primitives_short_method___operator_less_than_short_short'] = primitives_short_method___operator_less_than_short_short;
function primitives_half3_method___operator_double_multiply_half3_half3(__this, other) {
return [(__this[0] ** other[0]), (__this[1] ** other[1]), (__this[2] ** other[2])]
}

window['primitives_half3_method___operator_double_multiply_half3_half3'] = primitives_half3_method___operator_double_multiply_half3_half3;
function primitives_float3_method___operator_minus_float3_float(__this, other) {
return [(__this[0] - other), (__this[1] - other), (__this[2] - other)]
}

window['primitives_float3_method___operator_minus_float3_float'] = primitives_float3_method___operator_minus_float3_float;
function primitives_int2_method___operator_multiply_int2_int2(__this, other) {
return [(__this[0] * other[0]) & 0xffffffff, (__this[1] * other[1]) & 0xffffffff]
}

window['primitives_int2_method___operator_multiply_int2_int2'] = primitives_int2_method___operator_multiply_int2_int2;
function primitives_uint2_method___operator_divide_uint2_uint2(__this, other) {
return [(__this[0] / other[0])>>>0 & 0xffffffff, (__this[1] / other[1])>>>0 & 0xffffffff]
}

window['primitives_uint2_method___operator_divide_uint2_uint2'] = primitives_uint2_method___operator_divide_uint2_uint2;
function primitives_half4_method___operator_multiply_half4_half(__this, other) {
return [(__this[0] * other), (__this[1] * other), (__this[2] * other), (__this[3] * other)]
}

window['primitives_half4_method___operator_multiply_half4_half'] = primitives_half4_method___operator_multiply_half4_half;
function primitives_half2_method___operator_multiply_half2_half(__this, other) {
return [(__this[0] * other), (__this[1] * other)]
}

window['primitives_half2_method___operator_multiply_half2_half'] = primitives_half2_method___operator_multiply_half2_half;
function primitives_double4_method___operator_divide_double4_double4(__this, other) {
return [(__this[0] / other[0]), (__this[1] / other[1]), (__this[2] / other[2]), (__this[3] / other[3])]
}

window['primitives_double4_method___operator_divide_double4_double4'] = primitives_double4_method___operator_divide_double4_double4;
function primitives_short2_method___cast_from_vec_short2(other) {
return [(other[0] & 0xffff), (other[1] & 0xffff)];
}

window['primitives_short2_method___cast_from_vec_short2'] = primitives_short2_method___cast_from_vec_short2;
function primitives_float2_method___operator_divide_float2_float(__this, other) {
return [(__this[0] / other), (__this[1] / other)]
}

window['primitives_float2_method___operator_divide_float2_float'] = primitives_float2_method___operator_divide_float2_float;
function primitives_double2_method___operator_double_multiply_double2_double(__this, other) {
return [(__this[0] ** other), (__this[1] ** other)]
}

window['primitives_double2_method___operator_double_multiply_double2_double'] = primitives_double2_method___operator_double_multiply_double2_double;
function primitives_double3_method___operator_plus_double3_double(__this, other) {
return [(__this[0] + other), (__this[1] + other), (__this[2] + other)]
}

window['primitives_double3_method___operator_plus_double3_double'] = primitives_double3_method___operator_plus_double3_double;
function primitives_double_method___operator_equals_double_double(__this, other) {
return __this === other;
}

window['primitives_double_method___operator_equals_double_double'] = primitives_double_method___operator_equals_double_double;
function primitives_int_method___operator_not_equals_int_int(__this, other) {
return __this !== other;
}

window['primitives_int_method___operator_not_equals_int_int'] = primitives_int_method___operator_not_equals_int_int;
function primitives_short2_method___make_vec(x, y) {
return [x, y];
}

window['primitives_short2_method___make_vec'] = primitives_short2_method___make_vec;
function primitives_short3_method___operator_plus_short3_short3(__this, other) {
return [(__this[0] + other[0]) & 0xffff, (__this[1] + other[1]) & 0xffff, (__this[2] + other[2]) & 0xffff]
}

window['primitives_short3_method___operator_plus_short3_short3'] = primitives_short3_method___operator_plus_short3_short3;
function primitives_float3_method___prefix_operator_minus_float3(__this) {
return [(-__this[0]), (-__this[1]), (-__this[2])]
}

window['primitives_float3_method___prefix_operator_minus_float3'] = primitives_float3_method___prefix_operator_minus_float3;
function primitives_int4_method___operator_minus_int4_int(__this, other) {
return [(__this[0] - other) & 0xffffffff, (__this[1] - other) & 0xffffffff, (__this[2] - other) & 0xffffffff, (__this[3] - other) & 0xffffffff]
}

window['primitives_int4_method___operator_minus_int4_int'] = primitives_int4_method___operator_minus_int4_int;
function primitives_uint2_method___operator_minus_uint2_uint(__this, other) {
return [(__this[0] - other)>>>0 & 0xffffffff, (__this[1] - other)>>>0 & 0xffffffff]
}

window['primitives_uint2_method___operator_minus_uint2_uint'] = primitives_uint2_method___operator_minus_uint2_uint;
function primitives_double3_method___operator_divide_double3_double3(__this, other) {
return [(__this[0] / other[0]), (__this[1] / other[1]), (__this[2] / other[2])]
}

window['primitives_double3_method___operator_divide_double3_double3'] = primitives_double3_method___operator_divide_double3_double3;
function primitives_dist_int3_int3(a, b) {
return Math.sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y) + (a.z - b.z) * (a.z - b.z));
}

window['primitives_dist_int3_int3'] = primitives_dist_int3_int3;
function primitives_short_method___prefix_operator_join_short(__this) {
return (++__this);
}

window['primitives_short_method___prefix_operator_join_short'] = primitives_short_method___prefix_operator_join_short;
function primitives_uint2_method___make_vec(x, y) {
return [x, y];
}

window['primitives_uint2_method___make_vec'] = primitives_uint2_method___make_vec;
function primitives_half_method___operator_multiply_half_half(__this, other) {
return (__this * other);
}

window['primitives_half_method___operator_multiply_half_half'] = primitives_half_method___operator_multiply_half_half;
function primitives_uint2_method___operator_plus_uint2_uint(__this, other) {
return [(__this[0] + other)>>>0 & 0xffffffff, (__this[1] + other)>>>0 & 0xffffffff]
}

window['primitives_uint2_method___operator_plus_uint2_uint'] = primitives_uint2_method___operator_plus_uint2_uint;
function primitives_float_method___operator_divide_float_float(__this, other) {
return (__this / other);
}

window['primitives_float_method___operator_divide_float_float'] = primitives_float_method___operator_divide_float_float;
function primitives_int_method___prefix_operator_tilda_int(__this) {
return (~__this) & 0xffffffff;
}

window['primitives_int_method___prefix_operator_tilda_int'] = primitives_int_method___prefix_operator_tilda_int;
function primitives_uint2_method___operator_double_multiply_uint2_uint2(__this, other) {
return [(__this[0] ** other[0])>>>0 & 0xffffffff, (__this[1] ** other[1])>>>0 & 0xffffffff]
}

window['primitives_uint2_method___operator_double_multiply_uint2_uint2'] = primitives_uint2_method___operator_double_multiply_uint2_uint2;
function primitives_uint2_method___operator_modulo_uint2_uint2(__this, other) {
return [(__this[0] % other[0])>>>0 & 0xffffffff, (__this[1] % other[1])>>>0 & 0xffffffff]
}

window['primitives_uint2_method___operator_modulo_uint2_uint2'] = primitives_uint2_method___operator_modulo_uint2_uint2;
function primitives_double4_method___operator_plus_double4_double(__this, other) {
return [(__this[0] + other), (__this[1] + other), (__this[2] + other), (__this[3] + other)]
}

window['primitives_double4_method___operator_plus_double4_double'] = primitives_double4_method___operator_plus_double4_double;
function primitives_dist_half4_half4(a, b) {
return Math.sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y) + (a.z - b.z) * (a.z - b.z) + (a.w - b.w) * (a.w - b.w));
}

window['primitives_dist_half4_half4'] = primitives_dist_half4_half4;
function primitives_int3_method___operator_multiply_int3_int(__this, other) {
return [(__this[0] * other) & 0xffffffff, (__this[1] * other) & 0xffffffff, (__this[2] * other) & 0xffffffff]
}

window['primitives_int3_method___operator_multiply_int3_int'] = primitives_int3_method___operator_multiply_int3_int;
function primitives_int3_method___prefix_operator_minus_int3(__this) {
return [(-__this[0]) & 0xffffffff, (-__this[1]) & 0xffffffff, (-__this[2]) & 0xffffffff]
}

window['primitives_int3_method___prefix_operator_minus_int3'] = primitives_int3_method___prefix_operator_minus_int3;
function primitives_half3_method___operator_minus_half3_half3(__this, other) {
return [(__this[0] - other[0]), (__this[1] - other[1]), (__this[2] - other[2])]
}

window['primitives_half3_method___operator_minus_half3_half3'] = primitives_half3_method___operator_minus_half3_half3;
function primitives_uint2_method___operator_multiply_uint2_uint2(__this, other) {
return [(__this[0] * other[0])>>>0 & 0xffffffff, (__this[1] * other[1])>>>0 & 0xffffffff]
}

window['primitives_uint2_method___operator_multiply_uint2_uint2'] = primitives_uint2_method___operator_multiply_uint2_uint2;
function primitives_short3_method___operator_minus_short3_short(__this, other) {
return [(__this[0] - other) & 0xffff, (__this[1] - other) & 0xffff, (__this[2] - other) & 0xffff]
}

window['primitives_short3_method___operator_minus_short3_short'] = primitives_short3_method___operator_minus_short3_short;
function primitives_double2_method___make_vec(x, y) {
return [x, y];
}

window['primitives_double2_method___make_vec'] = primitives_double2_method___make_vec;
function primitives_half2_method___construct_half_half(x, y) {
return [x, y];
}

window['primitives_half2_method___construct_half_half'] = primitives_half2_method___construct_half_half;
function primitives_bool_method___operator_and_and_bool_bool(__this, other) {
return __this && other;
}

window['primitives_bool_method___operator_and_and_bool_bool'] = primitives_bool_method___operator_and_and_bool_bool;
function primitives_half_method___operator_not_equals_half_half(__this, other) {
return __this !== other;
}

window['primitives_half_method___operator_not_equals_half_half'] = primitives_half_method___operator_not_equals_half_half;
function primitives_dist_short4_short4(a, b) {
return Math.sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y) + (a.z - b.z) * (a.z - b.z) + (a.w - b.w) * (a.w - b.w));
}

window['primitives_dist_short4_short4'] = primitives_dist_short4_short4;
function primitives_half3_method___cast_from_vec_half3(other) {
return [(other[0]), (other[1]), (other[2])];
}

window['primitives_half3_method___cast_from_vec_half3'] = primitives_half3_method___cast_from_vec_half3;
function primitives_uint3_method___operator_plus_uint3_uint(__this, other) {
return [(__this[0] + other)>>>0 & 0xffffffff, (__this[1] + other)>>>0 & 0xffffffff, (__this[2] + other)>>>0 & 0xffffffff]
}

window['primitives_uint3_method___operator_plus_uint3_uint'] = primitives_uint3_method___operator_plus_uint3_uint;
function primitives_double_method___operator_less_than_double_double(__this, other) {
return __this < other;
}

window['primitives_double_method___operator_less_than_double_double'] = primitives_double_method___operator_less_than_double_double;
function primitives_int4_method___operator_divide_int4_int(__this, other) {
return [(__this[0] / other) & 0xffffffff, (__this[1] / other) & 0xffffffff, (__this[2] / other) & 0xffffffff, (__this[3] / other) & 0xffffffff]
}

window['primitives_int4_method___operator_divide_int4_int'] = primitives_int4_method___operator_divide_int4_int;
function primitives_uint_method___operator_double_multiply_uint_uint(__this, other) {
return (__this ** other)>>>0 & 0xffffffff;
}

window['primitives_uint_method___operator_double_multiply_uint_uint'] = primitives_uint_method___operator_double_multiply_uint_uint;
function primitives_half2_method___operator_minus_half2_half2(__this, other) {
return [(__this[0] - other[0]), (__this[1] - other[1])]
}

window['primitives_half2_method___operator_minus_half2_half2'] = primitives_half2_method___operator_minus_half2_half2;
function primitives_int_method___operator_equals_int_int(__this, other) {
return __this === other;
}

window['primitives_int_method___operator_equals_int_int'] = primitives_int_method___operator_equals_int_int;
function primitives_uint2_method___prefix_operator_minus_uint2(__this) {
return [(-__this[0])>>>0 & 0xffffffff, (-__this[1])>>>0 & 0xffffffff]
}

window['primitives_uint2_method___prefix_operator_minus_uint2'] = primitives_uint2_method___prefix_operator_minus_uint2;
function primitives_array_method___index_set_array___any____int_T0(__this, index, value) {
__this[index] = value;
}

window['primitives_array_method___index_set_array___any____int_T0'] = primitives_array_method___index_set_array___any____int_T0;
function primitives_bool_method___operator_not_equals_bool_bool(__this, other) {
return __this !== other;
}

window['primitives_bool_method___operator_not_equals_bool_bool'] = primitives_bool_method___operator_not_equals_bool_bool;
function primitives_dist_half3_half3(a, b) {
return Math.sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y) + (a.z - b.z) * (a.z - b.z));
}

window['primitives_dist_half3_half3'] = primitives_dist_half3_half3;
function primitives_short_method___operator_bar_short_short(__this, other) {
return (__this | other) & 0xffff;
}

window['primitives_short_method___operator_bar_short_short'] = primitives_short_method___operator_bar_short_short;
function primitives_double2_method___operator_divide_double2_double(__this, other) {
return [(__this[0] / other), (__this[1] / other)]
}

window['primitives_double2_method___operator_divide_double2_double'] = primitives_double2_method___operator_divide_double2_double;
function primitives_int3_method___is_vec_3_int3_int3(__this, other) {

}

window['primitives_int3_method___is_vec_3_int3_int3'] = primitives_int3_method___is_vec_3_int3_int3;
function primitives_int_method___is_scalar_int_int(__this, other) {

}

window['primitives_int_method___is_scalar_int_int'] = primitives_int_method___is_scalar_int_int;
function primitives_half2_method___cast_from_vec_half2(other) {
return [(other[0]), (other[1])];
}

window['primitives_half2_method___cast_from_vec_half2'] = primitives_half2_method___cast_from_vec_half2;
function primitives_half_method___operator_greater_than_or_equals_half_half(__this, other) {
return __this >= other;
}

window['primitives_half_method___operator_greater_than_or_equals_half_half'] = primitives_half_method___operator_greater_than_or_equals_half_half;
function primitives_half3_method___operator_double_multiply_half3_half(__this, other) {
return [(__this[0] ** other), (__this[1] ** other), (__this[2] ** other)]
}

window['primitives_half3_method___operator_double_multiply_half3_half'] = primitives_half3_method___operator_double_multiply_half3_half;
function primitives_int2_method___operator_double_multiply_int2_int2(__this, other) {
return [(__this[0] ** other[0]) & 0xffffffff, (__this[1] ** other[1]) & 0xffffffff]
}

window['primitives_int2_method___operator_double_multiply_int2_int2'] = primitives_int2_method___operator_double_multiply_int2_int2;
function primitives_int_method___operator_bar_int_int(__this, other) {
return (__this | other) & 0xffffffff;
}

window['primitives_int_method___operator_bar_int_int'] = primitives_int_method___operator_bar_int_int;
function primitives_int2_method___is_vec_2_int2_int2(__this, other) {

}

window['primitives_int2_method___is_vec_2_int2_int2'] = primitives_int2_method___is_vec_2_int2_int2;
function primitives_half2_method___operator_multiply_half2_half2(__this, other) {
return [(__this[0] * other[0]), (__this[1] * other[1])]
}

window['primitives_half2_method___operator_multiply_half2_half2'] = primitives_half2_method___operator_multiply_half2_half2;
function primitives_float4_method___prefix_operator_minus_float4(__this) {
return [(-__this[0]), (-__this[1]), (-__this[2]), (-__this[3])]
}

window['primitives_float4_method___prefix_operator_minus_float4'] = primitives_float4_method___prefix_operator_minus_float4;
function primitives_float3_method___operator_divide_float3_float3(__this, other) {
return [(__this[0] / other[0]), (__this[1] / other[1]), (__this[2] / other[2])]
}

window['primitives_float3_method___operator_divide_float3_float3'] = primitives_float3_method___operator_divide_float3_float3;
function primitives_double3_method___operator_cross_double3_double3(__this, other) {
return [(__this[0] * other[0]), (__this[1] * other[1]), (__this[2] * other[2])]
}

window['primitives_double3_method___operator_cross_double3_double3'] = primitives_double3_method___operator_cross_double3_double3;
function primitives_uint_method___cast_from_scalar_uint(other) {
return other>>>0 & 0xffffffff;
}

window['primitives_uint_method___cast_from_scalar_uint'] = primitives_uint_method___cast_from_scalar_uint;
function primitives_half4_method___is_vec_4_half4_half4(__this, other) {

}

window['primitives_half4_method___is_vec_4_half4_half4'] = primitives_half4_method___is_vec_4_half4_half4;
function primitives_float_method___postfix_operator_join_float(__this) {
return (__this++);
}

window['primitives_float_method___postfix_operator_join_float'] = primitives_float_method___postfix_operator_join_float;
function primitives_int3_method___operator_modulo_int3_int(__this, other) {
return [(__this[0] % other) & 0xffffffff, (__this[1] % other) & 0xffffffff, (__this[2] % other) & 0xffffffff]
}

window['primitives_int3_method___operator_modulo_int3_int'] = primitives_int3_method___operator_modulo_int3_int;
function primitives_float4_method___operator_plus_float4_float(__this, other) {
return [(__this[0] + other), (__this[1] + other), (__this[2] + other), (__this[3] + other)]
}

window['primitives_float4_method___operator_plus_float4_float'] = primitives_float4_method___operator_plus_float4_float;
function primitives_float2_method___operator_plus_float2_float(__this, other) {
return [(__this[0] + other), (__this[1] + other)]
}

window['primitives_float2_method___operator_plus_float2_float'] = primitives_float2_method___operator_plus_float2_float;
function primitives_half2_method___operator_cross_half2_half2(__this, other) {
return [(__this[0] * other[0]), (__this[1] * other[1])]
}

window['primitives_half2_method___operator_cross_half2_half2'] = primitives_half2_method___operator_cross_half2_half2;
function primitives_dist_double2_double2(a, b) {
return dist(a, b);
}

window['primitives_dist_double2_double2'] = primitives_dist_double2_double2;
function primitives_double2_method___construct_double_double(x, y) {
return [x, y];
}

window['primitives_double2_method___construct_double_double'] = primitives_double2_method___construct_double_double;
function primitives_float4_method___operator_minus_float4_float(__this, other) {
return [(__this[0] - other), (__this[1] - other), (__this[2] - other), (__this[3] - other)]
}

window['primitives_float4_method___operator_minus_float4_float'] = primitives_float4_method___operator_minus_float4_float;
function primitives_int4_method___operator_modulo_int4_int(__this, other) {
return [(__this[0] % other) & 0xffffffff, (__this[1] % other) & 0xffffffff, (__this[2] % other) & 0xffffffff, (__this[3] % other) & 0xffffffff]
}

window['primitives_int4_method___operator_modulo_int4_int'] = primitives_int4_method___operator_modulo_int4_int;
function primitives_uint_method___operator_and_uint_uint(__this, other) {
return (__this & other)>>>0 & 0xffffffff;
}

window['primitives_uint_method___operator_and_uint_uint'] = primitives_uint_method___operator_and_uint_uint;
function primitives_uint_method___operator_equals_uint_uint(__this, other) {
return __this === other;
}

window['primitives_uint_method___operator_equals_uint_uint'] = primitives_uint_method___operator_equals_uint_uint;
function primitives_double4_method___construct_double_double_double_double(x, y, z, w) {
return [x, y, z, w];
}

window['primitives_double4_method___construct_double_double_double_double'] = primitives_double4_method___construct_double_double_double_double;
function primitives_short4_method___operator_divide_short4_short4(__this, other) {
return [(__this[0] / other[0]) & 0xffff, (__this[1] / other[1]) & 0xffff, (__this[2] / other[2]) & 0xffff, (__this[3] / other[3]) & 0xffff]
}

window['primitives_short4_method___operator_divide_short4_short4'] = primitives_short4_method___operator_divide_short4_short4;
function primitives_int_method___operator_double_right_int_int(__this, other) {
return (__this >> other) & 0xffffffff;
}

window['primitives_int_method___operator_double_right_int_int'] = primitives_int_method___operator_double_right_int_int;
function primitives_short3_method___operator_minus_short3_short3(__this, other) {
return [(__this[0] - other[0]) & 0xffff, (__this[1] - other[1]) & 0xffff, (__this[2] - other[2]) & 0xffff]
}

window['primitives_short3_method___operator_minus_short3_short3'] = primitives_short3_method___operator_minus_short3_short3;
function primitives_int4_method___is_vec_4_int4_int4(__this, other) {

}

window['primitives_int4_method___is_vec_4_int4_int4'] = primitives_int4_method___is_vec_4_int4_int4;
function primitives_float2_method___is_vec_2_float2_float2(__this, other) {

}

window['primitives_float2_method___is_vec_2_float2_float2'] = primitives_float2_method___is_vec_2_float2_float2;
function primitives_array_method___index_array___any____int(__this, index) {
return __this[index];
}

window['primitives_array_method___index_array___any____int'] = primitives_array_method___index_array___any____int;
function primitives_double_method___operator_double_multiply_double_double(__this, other) {
return (__this ** other);
}

window['primitives_double_method___operator_double_multiply_double_double'] = primitives_double_method___operator_double_multiply_double_double;
function primitives_int4_method___make_vec(x, y, z, w) {
return [x, y, z, w];
}

window['primitives_int4_method___make_vec'] = primitives_int4_method___make_vec;
function primitives_short_method___operator_divide_short_short(__this, other) {
return (__this / other) & 0xffff;
}

window['primitives_short_method___operator_divide_short_short'] = primitives_short_method___operator_divide_short_short;
function primitives_float2_method___operator_modulo_float2_float2(__this, other) {
return [(__this[0] % other[0]), (__this[1] % other[1])]
}

window['primitives_float2_method___operator_modulo_float2_float2'] = primitives_float2_method___operator_modulo_float2_float2;
function __get_struct_test__shadeup_Player_health(struct) {
return struct.health;
}

window['__get_struct_test__shadeup_Player_health'] = __get_struct_test__shadeup_Player_health;
function primitives_float4_method___operator_divide_float4_float(__this, other) {
return [(__this[0] / other), (__this[1] / other), (__this[2] / other), (__this[3] / other)]
}

window['primitives_float4_method___operator_divide_float4_float'] = primitives_float4_method___operator_divide_float4_float;
function primitives_uint_method___prefix_operator_minus_uint(__this) {
return (-__this)>>>0 & 0xffffffff;
}

window['primitives_uint_method___prefix_operator_minus_uint'] = primitives_uint_method___prefix_operator_minus_uint;
function primitives_float4_method___operator_minus_float4_float4(__this, other) {
return [(__this[0] - other[0]), (__this[1] - other[1]), (__this[2] - other[2]), (__this[3] - other[3])]
}

window['primitives_float4_method___operator_minus_float4_float4'] = primitives_float4_method___operator_minus_float4_float4;
function primitives_int_method___operator_hat_int_int(__this, other) {
return (__this ^ other) & 0xffffffff;
}

window['primitives_int_method___operator_hat_int_int'] = primitives_int_method___operator_hat_int_int;
function primitives_double3_method___construct_double_double_double(x, y, z) {
return [x, y, z];
}

window['primitives_double3_method___construct_double_double_double'] = primitives_double3_method___construct_double_double_double;
function primitives_double4_method___operator_cross_double4_double4(__this, other) {
return [(__this[0] * other[0]), (__this[1] * other[1]), (__this[2] * other[2]), (__this[3] * other[3])]
}

window['primitives_double4_method___operator_cross_double4_double4'] = primitives_double4_method___operator_cross_double4_double4;
function primitives_dist_double4_double4(a, b) {
return Math.sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y) + (a.z - b.z) * (a.z - b.z) + (a.w - b.w) * (a.w - b.w));
}

window['primitives_dist_double4_double4'] = primitives_dist_double4_double4;
function primitives_int_method___postfix_operator_join_int(__this) {
return (__this++);
}

window['primitives_int_method___postfix_operator_join_int'] = primitives_int_method___postfix_operator_join_int;
function primitives_half2_method___operator_plus_half2_half2(__this, other) {
return [(__this[0] + other[0]), (__this[1] + other[1])]
}

window['primitives_half2_method___operator_plus_half2_half2'] = primitives_half2_method___operator_plus_half2_half2;
function primitives_half_method___prefix_operator_minus_half(__this) {
return (-__this);
}

window['primitives_half_method___prefix_operator_minus_half'] = primitives_half_method___prefix_operator_minus_half;
function primitives_float_method___operator_double_multiply_float_float(__this, other) {
return (__this ** other);
}

window['primitives_float_method___operator_double_multiply_float_float'] = primitives_float_method___operator_double_multiply_float_float;
function primitives_byte_method___operator_equals_byte_byte(__this, other) {
return __this === other;
}

window['primitives_byte_method___operator_equals_byte_byte'] = primitives_byte_method___operator_equals_byte_byte;
function primitives_int2_method___operator_plus_int2_int(__this, other) {
return [(__this[0] + other) & 0xffffffff, (__this[1] + other) & 0xffffffff]
}

window['primitives_int2_method___operator_plus_int2_int'] = primitives_int2_method___operator_plus_int2_int;
function primitives_half3_method___operator_divide_half3_half3(__this, other) {
return [(__this[0] / other[0]), (__this[1] / other[1]), (__this[2] / other[2])]
}

window['primitives_half3_method___operator_divide_half3_half3'] = primitives_half3_method___operator_divide_half3_half3;
function primitives_half_method___is_scalar_half_half(__this, other) {

}

window['primitives_half_method___is_scalar_half_half'] = primitives_half_method___is_scalar_half_half;
function primitives_double_method___operator_greater_than_double_double(__this, other) {
return __this > other;
}

window['primitives_double_method___operator_greater_than_double_double'] = primitives_double_method___operator_greater_than_double_double;
function primitives_float3_method___is_vec_3_float3_float3(__this, other) {

}

window['primitives_float3_method___is_vec_3_float3_float3'] = primitives_float3_method___is_vec_3_float3_float3;
function primitives_int2_method___operator_minus_int2_int2(__this, other) {
return [(__this[0] - other[0]) & 0xffffffff, (__this[1] - other[1]) & 0xffffffff]
}

window['primitives_int2_method___operator_minus_int2_int2'] = primitives_int2_method___operator_minus_int2_int2;
function primitives_double2_method___operator_double_multiply_double2_double2(__this, other) {
return [(__this[0] ** other[0]), (__this[1] ** other[1])]
}

window['primitives_double2_method___operator_double_multiply_double2_double2'] = primitives_double2_method___operator_double_multiply_double2_double2;
function primitives_half_method___postfix_operator_minus_minus_half(__this) {
return (__this--);
}

window['primitives_half_method___postfix_operator_minus_minus_half'] = primitives_half_method___postfix_operator_minus_minus_half;
function primitives_half4_method___operator_multiply_half4_half4(__this, other) {
return [(__this[0] * other[0]), (__this[1] * other[1]), (__this[2] * other[2]), (__this[3] * other[3])]
}

window['primitives_half4_method___operator_multiply_half4_half4'] = primitives_half4_method___operator_multiply_half4_half4;
function primitives_dist_short3_short3(a, b) {
return Math.sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y) + (a.z - b.z) * (a.z - b.z));
}

window['primitives_dist_short3_short3'] = primitives_dist_short3_short3;
function primitives_half4_method___operator_divide_half4_half(__this, other) {
return [(__this[0] / other), (__this[1] / other), (__this[2] / other), (__this[3] / other)]
}

window['primitives_half4_method___operator_divide_half4_half'] = primitives_half4_method___operator_divide_half4_half;
function primitives_short3_method___operator_divide_short3_short(__this, other) {
return [(__this[0] / other) & 0xffff, (__this[1] / other) & 0xffff, (__this[2] / other) & 0xffff]
}

window['primitives_short3_method___operator_divide_short3_short'] = primitives_short3_method___operator_divide_short3_short;
function primitives_int_method___operator_minus_int_int(__this, other) {
return (__this - other) & 0xffffffff;
}

window['primitives_int_method___operator_minus_int_int'] = primitives_int_method___operator_minus_int_int;
function primitives_half3_method___operator_divide_half3_half(__this, other) {
return [(__this[0] / other), (__this[1] / other), (__this[2] / other)]
}

window['primitives_half3_method___operator_divide_half3_half'] = primitives_half3_method___operator_divide_half3_half;
function primitives_double4_method___operator_divide_double4_double(__this, other) {
return [(__this[0] / other), (__this[1] / other), (__this[2] / other), (__this[3] / other)]
}

window['primitives_double4_method___operator_divide_double4_double'] = primitives_double4_method___operator_divide_double4_double;
function primitives_double2_method___operator_minus_double2_double2(__this, other) {
return [(__this[0] - other[0]), (__this[1] - other[1])]
}

window['primitives_double2_method___operator_minus_double2_double2'] = primitives_double2_method___operator_minus_double2_double2;
function primitives_bool_method___operator_equals_bool_bool(__this, other) {
return __this === other;
}

window['primitives_bool_method___operator_equals_bool_bool'] = primitives_bool_method___operator_equals_bool_bool;
function primitives_float2_method___operator_multiply_float2_float2(__this, other) {
return [(__this[0] * other[0]), (__this[1] * other[1])]
}

window['primitives_float2_method___operator_multiply_float2_float2'] = primitives_float2_method___operator_multiply_float2_float2;
function primitives_float_method___prefix_operator_minus_minus_float(__this) {
return (--__this);
}

window['primitives_float_method___prefix_operator_minus_minus_float'] = primitives_float_method___prefix_operator_minus_minus_float;
function primitives_short4_method___operator_multiply_short4_short4(__this, other) {
return [(__this[0] * other[0]) & 0xffff, (__this[1] * other[1]) & 0xffff, (__this[2] * other[2]) & 0xffff, (__this[3] * other[3]) & 0xffff]
}

window['primitives_short4_method___operator_multiply_short4_short4'] = primitives_short4_method___operator_multiply_short4_short4;
function primitives_short4_method___operator_double_multiply_short4_short4(__this, other) {
return [(__this[0] ** other[0]) & 0xffff, (__this[1] ** other[1]) & 0xffff, (__this[2] ** other[2]) & 0xffff, (__this[3] ** other[3]) & 0xffff]
}

window['primitives_short4_method___operator_double_multiply_short4_short4'] = primitives_short4_method___operator_double_multiply_short4_short4;
function primitives_short4_method___cast_from_vec_short4(other) {
return [(other[0] & 0xffff), (other[1] & 0xffff), (other[2] & 0xffff), (other[3] & 0xffff)];
}

window['primitives_short4_method___cast_from_vec_short4'] = primitives_short4_method___cast_from_vec_short4;
function primitives_int_method___operator_less_than_int_int(__this, other) {
return __this < other;
}

window['primitives_int_method___operator_less_than_int_int'] = primitives_int_method___operator_less_than_int_int;
function primitives_short4_method___operator_plus_short4_short4(__this, other) {
return [(__this[0] + other[0]) & 0xffff, (__this[1] + other[1]) & 0xffff, (__this[2] + other[2]) & 0xffff, (__this[3] + other[3]) & 0xffff]
}

window['primitives_short4_method___operator_plus_short4_short4'] = primitives_short4_method___operator_plus_short4_short4;
function primitives_float4_method___operator_multiply_float4_float(__this, other) {
return [(__this[0] * other), (__this[1] * other), (__this[2] * other), (__this[3] * other)]
}

window['primitives_float4_method___operator_multiply_float4_float'] = primitives_float4_method___operator_multiply_float4_float;
function primitives_int4_method___operator_double_multiply_int4_int(__this, other) {
return [(__this[0] ** other) & 0xffffffff, (__this[1] ** other) & 0xffffffff, (__this[2] ** other) & 0xffffffff, (__this[3] ** other) & 0xffffffff]
}

window['primitives_int4_method___operator_double_multiply_int4_int'] = primitives_int4_method___operator_double_multiply_int4_int;
function primitives_short3_method___operator_modulo_short3_short(__this, other) {
return [(__this[0] % other) & 0xffff, (__this[1] % other) & 0xffff, (__this[2] % other) & 0xffff]
}

window['primitives_short3_method___operator_modulo_short3_short'] = primitives_short3_method___operator_modulo_short3_short;
function primitives_half_method___prefix_operator_join_half(__this) {
return (++__this);
}

window['primitives_half_method___prefix_operator_join_half'] = primitives_half_method___prefix_operator_join_half;
function primitives_half_method___cast_from_scalar_half(other) {
return other;
}

window['primitives_half_method___cast_from_scalar_half'] = primitives_half_method___cast_from_scalar_half;
function primitives_half2_method___operator_divide_half2_half(__this, other) {
return [(__this[0] / other), (__this[1] / other)]
}

window['primitives_half2_method___operator_divide_half2_half'] = primitives_half2_method___operator_divide_half2_half;
function primitives_short2_method___operator_cross_short2_short2(__this, other) {
return [(__this[0] * other[0]) & 0xffff, (__this[1] * other[1]) & 0xffff]
}

window['primitives_short2_method___operator_cross_short2_short2'] = primitives_short2_method___operator_cross_short2_short2;
function primitives_short2_method___is_vec_2_short2_short2(__this, other) {

}

window['primitives_short2_method___is_vec_2_short2_short2'] = primitives_short2_method___is_vec_2_short2_short2;
function primitives_uint_method___operator_divide_uint_uint(__this, other) {
return (__this / other)>>>0 & 0xffffffff;
}

window['primitives_uint_method___operator_divide_uint_uint'] = primitives_uint_method___operator_divide_uint_uint;
function primitives_half4_method___operator_modulo_half4_half(__this, other) {
return [(__this[0] % other), (__this[1] % other), (__this[2] % other), (__this[3] % other)]
}

window['primitives_half4_method___operator_modulo_half4_half'] = primitives_half4_method___operator_modulo_half4_half;
function primitives_float2_method___prefix_operator_minus_float2(__this) {
return [(-__this[0]), (-__this[1])]
}

window['primitives_float2_method___prefix_operator_minus_float2'] = primitives_float2_method___prefix_operator_minus_float2;
function primitives_float4_method___operator_double_multiply_float4_float(__this, other) {
return [(__this[0] ** other), (__this[1] ** other), (__this[2] ** other), (__this[3] ** other)]
}

window['primitives_float4_method___operator_double_multiply_float4_float'] = primitives_float4_method___operator_double_multiply_float4_float;
function primitives_short2_method___operator_double_multiply_short2_short(__this, other) {
return [(__this[0] ** other) & 0xffff, (__this[1] ** other) & 0xffff]
}

window['primitives_short2_method___operator_double_multiply_short2_short'] = primitives_short2_method___operator_double_multiply_short2_short;
function primitives_double4_method___operator_minus_double4_double4(__this, other) {
return [(__this[0] - other[0]), (__this[1] - other[1]), (__this[2] - other[2]), (__this[3] - other[3])]
}

window['primitives_double4_method___operator_minus_double4_double4'] = primitives_double4_method___operator_minus_double4_double4;
function primitives_half4_method___operator_double_multiply_half4_half(__this, other) {
return [(__this[0] ** other), (__this[1] ** other), (__this[2] ** other), (__this[3] ** other)]
}

window['primitives_half4_method___operator_double_multiply_half4_half'] = primitives_half4_method___operator_double_multiply_half4_half;
function primitives_int4_method___operator_cross_int4_int4(__this, other) {
return [(__this[0] * other[0]) & 0xffffffff, (__this[1] * other[1]) & 0xffffffff, (__this[2] * other[2]) & 0xffffffff, (__this[3] * other[3]) & 0xffffffff]
}

window['primitives_int4_method___operator_cross_int4_int4'] = primitives_int4_method___operator_cross_int4_int4;
function primitives_short3_method___operator_double_multiply_short3_short(__this, other) {
return [(__this[0] ** other) & 0xffff, (__this[1] ** other) & 0xffff, (__this[2] ** other) & 0xffff]
}

window['primitives_short3_method___operator_double_multiply_short3_short'] = primitives_short3_method___operator_double_multiply_short3_short;
function primitives_string_method___operator_not_equals_string_string(__this, other) {
return __this !== other;
}

window['primitives_string_method___operator_not_equals_string_string'] = primitives_string_method___operator_not_equals_string_string;
function primitives_int2_method___cast_from_vec_int2(other) {
return [(other[0] & 0xffffffff), (other[1] & 0xffffffff)];
}

window['primitives_int2_method___cast_from_vec_int2'] = primitives_int2_method___cast_from_vec_int2;
function primitives_float_method___postfix_operator_minus_minus_float(__this) {
return (__this--);
}

window['primitives_float_method___postfix_operator_minus_minus_float'] = primitives_float_method___postfix_operator_minus_minus_float;
function primitives_double_method___operator_less_than_or_equals_double_double(__this, other) {
return __this <= other;
}

window['primitives_double_method___operator_less_than_or_equals_double_double'] = primitives_double_method___operator_less_than_or_equals_double_double;

Parse: 7.7604ms
Process: 873.9┬╡s
Generate: 108.7512ms
Total: 154.1675ms
