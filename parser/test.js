function __make_struct_test__shadeup_Test(fields) {
  return { a: fields.a, b: fields.b };
}
function __get_struct_test__shadeup_Test_a(struct) {
  return struct.a;
}
function __get_struct_test__shadeup_Test_b(struct) {
  return struct.b;
}
function __uint2___operator_multiply(__this, other) {
  return [
    ((__this[0] * other[0]) >>> 0) & 0xffffffff,
    ((__this[1] * other[1]) >>> 0) & 0xffffffff,
  ];
}
function test__shadeup_test() {
  let a = 1 & 0xffffffff;
  let b = __make_struct_test__shadeup_Test({
    a: 2 & 0xffffffff,
    b: 2 & 0xffffffff,
  });
  let c = __int___operator_plus(__get_struct_test__shadeup_Test_a(b), 2);
  if (__int___operator_greater_than_or_equals(a, 1 & 0xffffffff)) {
    let d = 2 & 0xffffffff;
    return 3 & 0xffffffff;
  }
  if (true) {
    return 1 & 0xffffffff;
  } else {
    // return 2 & 0xffffffff;
  }
}
function __short2___operator_plus(__this, other) {
  return [(__this[0] + other[0]) & 0xffff, (__this[1] + other[1]) & 0xffff];
}
function __int4___operator_divide(__this, other) {
  return [
    (__this[0] / other[0]) & 0xffffffff,
    (__this[1] / other[1]) & 0xffffffff,
    (__this[2] / other[2]) & 0xffffffff,
    (__this[3] / other[3]) & 0xffffffff,
  ];
}
function __uint___operator_minus(__this, other) {
  return ((__this - other) >>> 0) & 0xffffffff;
}
function __uint___operator_greater_than_or_equals(__this, other) {
  return __this >= other;
}
function __float2___operator_divide(__this, other) {
  return [__this[0] / other[0], __this[1] / other[1]];
}
function __float___operator_equals(__this, other) {
  return __this === other;
}
function __int2___operator_cross(__this, other) {
  return [
    (__this[0] * other[0]) & 0xffffffff,
    (__this[1] * other[1]) & 0xffffffff,
  ];
}
function __short3___operator_multiply(__this, other) {
  return [
    (__this[0] * other[0]) & 0xffff,
    (__this[1] * other[1]) & 0xffff,
    (__this[2] * other[2]) & 0xffff,
  ];
}
function __short2___operator_minus(__this, other) {
  return [(__this[0] - other[0]) & 0xffff, (__this[1] - other[1]) & 0xffff];
}
function __uint___operator_less_than(__this, other) {
  return __this < other;
}
function __double2___operator_plus(__this, other) {
  return [__this[0] + other[0], __this[1] + other[1]];
}
function __half___operator_plus(__this, other) {
  return __this + other;
}
function __double___operator_greater_than_or_equals(__this, other) {
  return __this >= other;
}
function __float3___operator_minus(__this, other) {
  return [__this[0] - other[0], __this[1] - other[1], __this[2] - other[2]];
}
function __int4___operator_cross(__this, other) {
  return [
    (__this[0] * other[0]) & 0xffffffff,
    (__this[1] * other[1]) & 0xffffffff,
    (__this[2] * other[2]) & 0xffffffff,
    (__this[3] * other[3]) & 0xffffffff,
  ];
}
function __int2___operator_modulo(__this, other) {
  return [__this[0] % other[0] & 0xffffffff, __this[1] % other[1] & 0xffffffff];
}
function __double4___operator_divide(__this, other) {
  return [
    __this[0] / other[0],
    __this[1] / other[1],
    __this[2] / other[2],
    __this[3] / other[3],
  ];
}
function __double___operator_multiply(__this, other) {
  return __this * other;
}
function __short___operator_less_than_or_equals(__this, other) {
  return __this <= other;
}
function __float___is_scalar(__this, other) {}
function __half2___operator_modulo(__this, other) {
  return [__this[0] % other[0], __this[1] % other[1]];
}
function __uint3___operator_plus(__this, other) {
  return [
    ((__this[0] + other[0]) >>> 0) & 0xffffffff,
    ((__this[1] + other[1]) >>> 0) & 0xffffffff,
    ((__this[2] + other[2]) >>> 0) & 0xffffffff,
  ];
}
function __uint3___operator_divide(__this, other) {
  return [
    ((__this[0] / other[0]) >>> 0) & 0xffffffff,
    ((__this[1] / other[1]) >>> 0) & 0xffffffff,
    ((__this[2] / other[2]) >>> 0) & 0xffffffff,
  ];
}
function __short4___operator_plus(__this, other) {
  return [
    (__this[0] + other[0]) & 0xffff,
    (__this[1] + other[1]) & 0xffff,
    (__this[2] + other[2]) & 0xffff,
    (__this[3] + other[3]) & 0xffff,
  ];
}
function __short4___operator_multiply(__this, other) {
  return [
    (__this[0] * other[0]) & 0xffff,
    (__this[1] * other[1]) & 0xffff,
    (__this[2] * other[2]) & 0xffff,
    (__this[3] * other[3]) & 0xffff,
  ];
}
function __short___operator_less_than(__this, other) {
  return __this < other;
}
function __uint4___operator_modulo(__this, other) {
  return [
    (__this[0] % other[0] >>> 0) & 0xffffffff,
    (__this[1] % other[1] >>> 0) & 0xffffffff,
    (__this[2] % other[2] >>> 0) & 0xffffffff,
    (__this[3] % other[3] >>> 0) & 0xffffffff,
  ];
}
function __int4___operator_minus(__this, other) {
  return [
    (__this[0] - other[0]) & 0xffffffff,
    (__this[1] - other[1]) & 0xffffffff,
    (__this[2] - other[2]) & 0xffffffff,
    (__this[3] - other[3]) & 0xffffffff,
  ];
}
function __int4___cast_from_vec(other) {
  return [
    other[0] & 0xffffffff,
    other[1] & 0xffffffff,
    other[2] & 0xffffffff,
    other[3] & 0xffffffff,
  ];
}
function __double___operator_plus(__this, other) {
  return __this + other;
}
function __uint___operator_multiply(__this, other) {
  return ((__this * other) >>> 0) & 0xffffffff;
}
function __int___operator_multiply(__this, other) {
  return (__this * other) & 0xffffffff;
}
function __int3___operator_minus(__this, other) {
  return [
    (__this[0] - other[0]) & 0xffffffff,
    (__this[1] - other[1]) & 0xffffffff,
    (__this[2] - other[2]) & 0xffffffff,
  ];
}
function __uint___cast_from_scalar(other) {
  return (other >>> 0) & 0xffffffff;
}
function __double___operator_modulo(__this, other) {
  return __this % other;
}
function __float4___operator_modulo(__this, other) {
  return [
    __this[0] % other[0],
    __this[1] % other[1],
    __this[2] % other[2],
    __this[3] % other[3],
  ];
}
function __half___cast_from_scalar(other) {
  return other;
}
function __int3___is_vec_3(__this, other) {}
function __short___operator_greater_than(__this, other) {
  return __this > other;
}
function __double___operator_not_equals(__this, other) {
  return __this !== other;
}
function __double2___operator_multiply(__this, other) {
  return [__this[0] * other[0], __this[1] * other[1]];
}
function __double___operator_equals(__this, other) {
  return __this === other;
}
function __float___operator_plus(__this, other) {
  return __this + other;
}
function __float___operator_less_than(__this, other) {
  return __this < other;
}
function __float3___operator_modulo(__this, other) {
  return [__this[0] % other[0], __this[1] % other[1], __this[2] % other[2]];
}
function __short3___operator_minus(__this, other) {
  return [
    (__this[0] - other[0]) & 0xffff,
    (__this[1] - other[1]) & 0xffff,
    (__this[2] - other[2]) & 0xffff,
  ];
}
function __uint3___is_vec_3(__this, other) {}
function __int___operator_minus(__this, other) {
  return (__this - other) & 0xffffffff;
}
function __double___is_scalar(__this, other) {}
function __uint___is_scalar(__this, other) {}
function __half3___cast_from_vec(other) {
  return [other[0], other[1], other[2]];
}
function __double___operator_divide(__this, other) {
  return __this / other;
}
function __int___operator_equals(__this, other) {
  return __this === other;
}
function __int3___operator_cross(__this, other) {
  return [
    (__this[0] * other[0]) & 0xffffffff,
    (__this[1] * other[1]) & 0xffffffff,
    (__this[2] * other[2]) & 0xffffffff,
  ];
}
function __int___operator_modulo(__this, other) {
  return __this % other & 0xffffffff;
}
function __float___cast_from_scalar(other) {
  return other;
}
function __half3___operator_cross(__this, other) {
  return [__this[0] * other[0], __this[1] * other[1], __this[2] * other[2]];
}
function __double3___operator_plus(__this, other) {
  return [__this[0] + other[0], __this[1] + other[1], __this[2] + other[2]];
}
function __short___operator_multiply(__this, other) {
  return (__this * other) & 0xffff;
}
function __uint3___operator_minus(__this, other) {
  return [
    ((__this[0] - other[0]) >>> 0) & 0xffffffff,
    ((__this[1] - other[1]) >>> 0) & 0xffffffff,
    ((__this[2] - other[2]) >>> 0) & 0xffffffff,
  ];
}
function __uint3___cast_from_vec(other) {
  return [
    (other[0] >>> 0) & 0xffffffff,
    (other[1] >>> 0) & 0xffffffff,
    (other[2] >>> 0) & 0xffffffff,
  ];
}
function __half___operator_less_than_or_equals(__this, other) {
  return __this <= other;
}
function __double3___operator_multiply(__this, other) {
  return [__this[0] * other[0], __this[1] * other[1], __this[2] * other[2]];
}
function __int4___operator_modulo(__this, other) {
  return [
    __this[0] % other[0] & 0xffffffff,
    __this[1] % other[1] & 0xffffffff,
    __this[2] % other[2] & 0xffffffff,
    __this[3] % other[3] & 0xffffffff,
  ];
}
function __double___cast_from_scalar(other) {
  return other;
}
function __double___operator_less_than_or_equals(__this, other) {
  return __this <= other;
}
function __short___is_scalar(__this, other) {}
function __float2___is_vec_2(__this, other) {}
function __short2___operator_cross(__this, other) {
  return [(__this[0] * other[0]) & 0xffff, (__this[1] * other[1]) & 0xffff];
}
function __half4___operator_modulo(__this, other) {
  return [
    __this[0] % other[0],
    __this[1] % other[1],
    __this[2] % other[2],
    __this[3] % other[3],
  ];
}
function __short3___is_vec_3(__this, other) {}
function __double2___operator_minus(__this, other) {
  return [__this[0] - other[0], __this[1] - other[1]];
}
function __double2___cast_from_vec(other) {
  return [other[0], other[1]];
}
function __double3___is_vec_3(__this, other) {}
function __int3___operator_plus(__this, other) {
  return [
    (__this[0] + other[0]) & 0xffffffff,
    (__this[1] + other[1]) & 0xffffffff,
    (__this[2] + other[2]) & 0xffffffff,
  ];
}
function __double4___cast_from_vec(other) {
  return [other[0], other[1], other[2], other[3]];
}
function __uint___operator_plus(__this, other) {
  return ((__this + other) >>> 0) & 0xffffffff;
}
function __half4___operator_divide(__this, other) {
  return [
    __this[0] / other[0],
    __this[1] / other[1],
    __this[2] / other[2],
    __this[3] / other[3],
  ];
}
function __half2___operator_multiply(__this, other) {
  return [__this[0] * other[0], __this[1] * other[1]];
}
function __half___is_scalar(__this, other) {}
function __int2___operator_minus(__this, other) {
  return [
    (__this[0] - other[0]) & 0xffffffff,
    (__this[1] - other[1]) & 0xffffffff,
  ];
}
function __double___operator_greater_than(__this, other) {
  return __this > other;
}
function __uint2___operator_modulo(__this, other) {
  return [
    (__this[0] % other[0] >>> 0) & 0xffffffff,
    (__this[1] % other[1] >>> 0) & 0xffffffff,
  ];
}
function __uint2___cast_from_vec(other) {
  return [(other[0] >>> 0) & 0xffffffff, (other[1] >>> 0) & 0xffffffff];
}
function __float3___is_vec_3(__this, other) {}
function __uint4___operator_minus(__this, other) {
  return [
    ((__this[0] - other[0]) >>> 0) & 0xffffffff,
    ((__this[1] - other[1]) >>> 0) & 0xffffffff,
    ((__this[2] - other[2]) >>> 0) & 0xffffffff,
    ((__this[3] - other[3]) >>> 0) & 0xffffffff,
  ];
}
function __double4___operator_cross(__this, other) {
  return [
    __this[0] * other[0],
    __this[1] * other[1],
    __this[2] * other[2],
    __this[3] * other[3],
  ];
}
function __short3___cast_from_vec(other) {
  return [other[0] & 0xffff, other[1] & 0xffff, other[2] & 0xffff];
}
function __short4___cast_from_vec(other) {
  return [
    other[0] & 0xffff,
    other[1] & 0xffff,
    other[2] & 0xffff,
    other[3] & 0xffff,
  ];
}
function __int___is_scalar(__this, other) {}
function __double2___operator_divide(__this, other) {
  return [__this[0] / other[0], __this[1] / other[1]];
}
function __half___operator_equals(__this, other) {
  return __this === other;
}
function __float___operator_greater_than_or_equals(__this, other) {
  return __this >= other;
}
function __float3___operator_multiply(__this, other) {
  return [__this[0] * other[0], __this[1] * other[1], __this[2] * other[2]];
}
function __int___operator_not_equals(__this, other) {
  return __this !== other;
}
function __int___operator_greater_than(__this, other) {
  return __this > other;
}
function __short4___is_vec_4(__this, other) {}
function __short3___operator_cross(__this, other) {
  return [
    (__this[0] * other[0]) & 0xffff,
    (__this[1] * other[1]) & 0xffff,
    (__this[2] * other[2]) & 0xffff,
  ];
}
function __uint4___cast_from_vec(other) {
  return [
    (other[0] >>> 0) & 0xffffffff,
    (other[1] >>> 0) & 0xffffffff,
    (other[2] >>> 0) & 0xffffffff,
    (other[3] >>> 0) & 0xffffffff,
  ];
}
function __float4___operator_plus(__this, other) {
  return [
    __this[0] + other[0],
    __this[1] + other[1],
    __this[2] + other[2],
    __this[3] + other[3],
  ];
}
function __int4___is_vec_4(__this, other) {}
function __uint___operator_equals(__this, other) {
  return __this === other;
}
function __short___operator_minus(__this, other) {
  return (__this - other) & 0xffff;
}
function __float3___cast_from_vec(other) {
  return [other[0], other[1], other[2]];
}
function __short___cast_from_scalar(other) {
  return other & 0xffff;
}
function __int3___operator_divide(__this, other) {
  return [
    (__this[0] / other[0]) & 0xffffffff,
    (__this[1] / other[1]) & 0xffffffff,
    (__this[2] / other[2]) & 0xffffffff,
  ];
}
function __int3___cast_from_vec(other) {
  return [other[0] & 0xffffffff, other[1] & 0xffffffff, other[2] & 0xffffffff];
}
function __int2___operator_plus(__this, other) {
  return [
    (__this[0] + other[0]) & 0xffffffff,
    (__this[1] + other[1]) & 0xffffffff,
  ];
}
function __half___operator_not_equals(__this, other) {
  return __this !== other;
}
function __half2___cast_from_vec(other) {
  return [other[0], other[1]];
}
function __float4___operator_cross(__this, other) {
  return [
    __this[0] * other[0],
    __this[1] * other[1],
    __this[2] * other[2],
    __this[3] * other[3],
  ];
}
function __uint___operator_less_than_or_equals(__this, other) {
  return __this <= other;
}
function __int2___cast_from_vec(other) {
  return [other[0] & 0xffffffff, other[1] & 0xffffffff];
}
function __short2___operator_modulo(__this, other) {
  return [__this[0] % other[0] & 0xffff, __this[1] % other[1] & 0xffff];
}
function __half___operator_modulo(__this, other) {
  return __this % other;
}
function __uint4___operator_multiply(__this, other) {
  return [
    ((__this[0] * other[0]) >>> 0) & 0xffffffff,
    ((__this[1] * other[1]) >>> 0) & 0xffffffff,
    ((__this[2] * other[2]) >>> 0) & 0xffffffff,
    ((__this[3] * other[3]) >>> 0) & 0xffffffff,
  ];
}
function __uint4___is_vec_4(__this, other) {}
function __half2___operator_cross(__this, other) {
  return [__this[0] * other[0], __this[1] * other[1]];
}
function __float4___operator_multiply(__this, other) {
  return [
    __this[0] * other[0],
    __this[1] * other[1],
    __this[2] * other[2],
    __this[3] * other[3],
  ];
}
function __uint3___operator_modulo(__this, other) {
  return [
    (__this[0] % other[0] >>> 0) & 0xffffffff,
    (__this[1] % other[1] >>> 0) & 0xffffffff,
    (__this[2] % other[2] >>> 0) & 0xffffffff,
  ];
}
function __int___operator_less_than(__this, other) {
  return __this < other;
}
function __double3___operator_cross(__this, other) {
  return [__this[0] * other[0], __this[1] * other[1], __this[2] * other[2]];
}
function __float2___operator_multiply(__this, other) {
  return [__this[0] * other[0], __this[1] * other[1]];
}
function __int2___operator_divide(__this, other) {
  return [
    (__this[0] / other[0]) & 0xffffffff,
    (__this[1] / other[1]) & 0xffffffff,
  ];
}
function __short___operator_divide(__this, other) {
  return (__this / other) & 0xffff;
}
function __uint___operator_greater_than(__this, other) {
  return __this > other;
}
function __short___operator_greater_than_or_equals(__this, other) {
  return __this >= other;
}
function __half4___cast_from_vec(other) {
  return [other[0], other[1], other[2], other[3]];
}
function __double3___operator_minus(__this, other) {
  return [__this[0] - other[0], __this[1] - other[1], __this[2] - other[2]];
}
function __double4___operator_multiply(__this, other) {
  return [
    __this[0] * other[0],
    __this[1] * other[1],
    __this[2] * other[2],
    __this[3] * other[3],
  ];
}
function __float___operator_not_equals(__this, other) {
  return __this !== other;
}
function __short___operator_equals(__this, other) {
  return __this === other;
}
function __float3___operator_plus(__this, other) {
  return [__this[0] + other[0], __this[1] + other[1], __this[2] + other[2]];
}
function __int2___is_vec_2(__this, other) {}
function __uint2___operator_divide(__this, other) {
  return [
    ((__this[0] / other[0]) >>> 0) & 0xffffffff,
    ((__this[1] / other[1]) >>> 0) & 0xffffffff,
  ];
}
function __double4___is_vec_4(__this, other) {}
function __half___operator_greater_than(__this, other) {
  return __this > other;
}
function __short2___is_vec_2(__this, other) {}
function __uint___operator_modulo(__this, other) {
  return (__this % other >>> 0) & 0xffffffff;
}
function __double3___operator_modulo(__this, other) {
  return [__this[0] % other[0], __this[1] % other[1], __this[2] % other[2]];
}
function __uint3___operator_multiply(__this, other) {
  return [
    ((__this[0] * other[0]) >>> 0) & 0xffffffff,
    ((__this[1] * other[1]) >>> 0) & 0xffffffff,
    ((__this[2] * other[2]) >>> 0) & 0xffffffff,
  ];
}
function __int___cast_from_scalar(other) {
  return other & 0xffffffff;
}
function __double___operator_less_than(__this, other) {
  return __this < other;
}
function __half4___operator_multiply(__this, other) {
  return [
    __this[0] * other[0],
    __this[1] * other[1],
    __this[2] * other[2],
    __this[3] * other[3],
  ];
}
function __short2___operator_multiply(__this, other) {
  return [(__this[0] * other[0]) & 0xffff, (__this[1] * other[1]) & 0xffff];
}
function __half2___operator_divide(__this, other) {
  return [__this[0] / other[0], __this[1] / other[1]];
}
function __float___operator_greater_than(__this, other) {
  return __this > other;
}
function __int4___operator_plus(__this, other) {
  return [
    (__this[0] + other[0]) & 0xffffffff,
    (__this[1] + other[1]) & 0xffffffff,
    (__this[2] + other[2]) & 0xffffffff,
    (__this[3] + other[3]) & 0xffffffff,
  ];
}
function __short2___cast_from_vec(other) {
  return [other[0] & 0xffff, other[1] & 0xffff];
}
function __int___operator_divide(__this, other) {
  return (__this / other) & 0xffffffff;
}
function __float___operator_multiply(__this, other) {
  return __this * other;
}
function __float4___cast_from_vec(other) {
  return [other[0], other[1], other[2], other[3]];
}
function __uint2___operator_cross(__this, other) {
  return [
    ((__this[0] * other[0]) >>> 0) & 0xffffffff,
    ((__this[1] * other[1]) >>> 0) & 0xffffffff,
  ];
}
function __uint___operator_not_equals(__this, other) {
  return __this !== other;
}
function __double2___operator_cross(__this, other) {
  return [__this[0] * other[0], __this[1] * other[1]];
}
function __half4___operator_minus(__this, other) {
  return [
    __this[0] - other[0],
    __this[1] - other[1],
    __this[2] - other[2],
    __this[3] - other[3],
  ];
}
function __half3___operator_plus(__this, other) {
  return [__this[0] + other[0], __this[1] + other[1], __this[2] + other[2]];
}
function __double3___cast_from_vec(other) {
  return [other[0], other[1], other[2]];
}
function __int3___operator_multiply(__this, other) {
  return [
    (__this[0] * other[0]) & 0xffffffff,
    (__this[1] * other[1]) & 0xffffffff,
    (__this[2] * other[2]) & 0xffffffff,
  ];
}
function __int2___operator_multiply(__this, other) {
  return [
    (__this[0] * other[0]) & 0xffffffff,
    (__this[1] * other[1]) & 0xffffffff,
  ];
}
function __double2___operator_modulo(__this, other) {
  return [__this[0] % other[0], __this[1] % other[1]];
}
function __int___operator_less_than_or_equals(__this, other) {
  return __this <= other;
}
function __uint3___operator_cross(__this, other) {
  return [
    ((__this[0] * other[0]) >>> 0) & 0xffffffff,
    ((__this[1] * other[1]) >>> 0) & 0xffffffff,
    ((__this[2] * other[2]) >>> 0) & 0xffffffff,
  ];
}
function __short4___operator_minus(__this, other) {
  return [
    (__this[0] - other[0]) & 0xffff,
    (__this[1] - other[1]) & 0xffff,
    (__this[2] - other[2]) & 0xffff,
    (__this[3] - other[3]) & 0xffff,
  ];
}
function __float4___operator_minus(__this, other) {
  return [
    __this[0] - other[0],
    __this[1] - other[1],
    __this[2] - other[2],
    __this[3] - other[3],
  ];
}
function __half3___operator_divide(__this, other) {
  return [__this[0] / other[0], __this[1] / other[1], __this[2] / other[2]];
}
function __float2___operator_minus(__this, other) {
  return [__this[0] - other[0], __this[1] - other[1]];
}
function __short___operator_not_equals(__this, other) {
  return __this !== other;
}
function __short3___operator_plus(__this, other) {
  return [
    (__this[0] + other[0]) & 0xffff,
    (__this[1] + other[1]) & 0xffff,
    (__this[2] + other[2]) & 0xffff,
  ];
}
function __double4___operator_modulo(__this, other) {
  return [
    __this[0] % other[0],
    __this[1] % other[1],
    __this[2] % other[2],
    __this[3] % other[3],
  ];
}
function __half___operator_divide(__this, other) {
  return __this / other;
}
function __half3___operator_minus(__this, other) {
  return [__this[0] - other[0], __this[1] - other[1], __this[2] - other[2]];
}
function __float3___operator_divide(__this, other) {
  return [__this[0] / other[0], __this[1] / other[1], __this[2] / other[2]];
}
function __double4___operator_minus(__this, other) {
  return [
    __this[0] - other[0],
    __this[1] - other[1],
    __this[2] - other[2],
    __this[3] - other[3],
  ];
}
function __uint2___is_vec_2(__this, other) {}
function __uint___operator_divide(__this, other) {
  return ((__this / other) >>> 0) & 0xffffffff;
}
function __half3___is_vec_3(__this, other) {}
function __half3___operator_modulo(__this, other) {
  return [__this[0] % other[0], __this[1] % other[1], __this[2] % other[2]];
}
function __int4___operator_multiply(__this, other) {
  return [
    (__this[0] * other[0]) & 0xffffffff,
    (__this[1] * other[1]) & 0xffffffff,
    (__this[2] * other[2]) & 0xffffffff,
    (__this[3] * other[3]) & 0xffffffff,
  ];
}
function __half4___operator_plus(__this, other) {
  return [
    __this[0] + other[0],
    __this[1] + other[1],
    __this[2] + other[2],
    __this[3] + other[3],
  ];
}
function __float___operator_less_than_or_equals(__this, other) {
  return __this <= other;
}
function __half2___is_vec_2(__this, other) {}
function __short4___operator_modulo(__this, other) {
  return [
    __this[0] % other[0] & 0xffff,
    __this[1] % other[1] & 0xffff,
    __this[2] % other[2] & 0xffff,
    __this[3] % other[3] & 0xffff,
  ];
}
function __float___operator_divide(__this, other) {
  return __this / other;
}
function other__shadeup_add() {
  let a = 1 & 0xffffffff;
  let b = 2 & 0xffffffff;
  let c = __int___operator_plus(a, b);
  return c;
}
function __short4___operator_divide(__this, other) {
  return [
    (__this[0] / other[0]) & 0xffff,
    (__this[1] / other[1]) & 0xffff,
    (__this[2] / other[2]) & 0xffff,
    (__this[3] / other[3]) & 0xffff,
  ];
}
function __short3___operator_divide(__this, other) {
  return [
    (__this[0] / other[0]) & 0xffff,
    (__this[1] / other[1]) & 0xffff,
    (__this[2] / other[2]) & 0xffff,
  ];
}
function __half___operator_multiply(__this, other) {
  return __this * other;
}
function __short4___operator_cross(__this, other) {
  return [
    (__this[0] * other[0]) & 0xffff,
    (__this[1] * other[1]) & 0xffff,
    (__this[2] * other[2]) & 0xffff,
    (__this[3] * other[3]) & 0xffff,
  ];
}
function __float2___operator_cross(__this, other) {
  return [__this[0] * other[0], __this[1] * other[1]];
}
function __uint4___operator_divide(__this, other) {
  return [
    ((__this[0] / other[0]) >>> 0) & 0xffffffff,
    ((__this[1] / other[1]) >>> 0) & 0xffffffff,
    ((__this[2] / other[2]) >>> 0) & 0xffffffff,
    ((__this[3] / other[3]) >>> 0) & 0xffffffff,
  ];
}
function __half2___operator_minus(__this, other) {
  return [__this[0] - other[0], __this[1] - other[1]];
}
function __float3___operator_cross(__this, other) {
  return [__this[0] * other[0], __this[1] * other[1], __this[2] * other[2]];
}
function __float2___cast_from_vec(other) {
  return [other[0], other[1]];
}
function __float4___is_vec_4(__this, other) {}
function __short___operator_modulo(__this, other) {
  return __this % other & 0xffff;
}
function __double3___operator_divide(__this, other) {
  return [__this[0] / other[0], __this[1] / other[1], __this[2] / other[2]];
}
function __double2___is_vec_2(__this, other) {}
function __half___operator_greater_than_or_equals(__this, other) {
  return __this >= other;
}
function __short2___operator_divide(__this, other) {
  return [(__this[0] / other[0]) & 0xffff, (__this[1] / other[1]) & 0xffff];
}
function __float4___operator_divide(__this, other) {
  return [
    __this[0] / other[0],
    __this[1] / other[1],
    __this[2] / other[2],
    __this[3] / other[3],
  ];
}
function __half3___operator_multiply(__this, other) {
  return [__this[0] * other[0], __this[1] * other[1], __this[2] * other[2]];
}
function __float___operator_modulo(__this, other) {
  return __this % other;
}
function __short3___operator_modulo(__this, other) {
  return [
    __this[0] % other[0] & 0xffff,
    __this[1] % other[1] & 0xffff,
    __this[2] % other[2] & 0xffff,
  ];
}
function __float___operator_minus(__this, other) {
  return __this - other;
}
function __int___operator_plus(__this, other) {
  return (__this + other) & 0xffffffff;
}
function __double4___operator_plus(__this, other) {
  return [
    __this[0] + other[0],
    __this[1] + other[1],
    __this[2] + other[2],
    __this[3] + other[3],
  ];
}
function __half2___operator_plus(__this, other) {
  return [__this[0] + other[0], __this[1] + other[1]];
}
function __float2___operator_plus(__this, other) {
  return [__this[0] + other[0], __this[1] + other[1]];
}
function __int___operator_greater_than_or_equals(__this, other) {
  return __this >= other;
}
function __half4___is_vec_4(__this, other) {}
function __half___operator_less_than(__this, other) {
  return __this < other;
}
function __half___operator_minus(__this, other) {
  return __this - other;
}
function __uint4___operator_cross(__this, other) {
  return [
    ((__this[0] * other[0]) >>> 0) & 0xffffffff,
    ((__this[1] * other[1]) >>> 0) & 0xffffffff,
    ((__this[2] * other[2]) >>> 0) & 0xffffffff,
    ((__this[3] * other[3]) >>> 0) & 0xffffffff,
  ];
}
function __double___operator_minus(__this, other) {
  return __this - other;
}
function __half4___operator_cross(__this, other) {
  return [
    __this[0] * other[0],
    __this[1] * other[1],
    __this[2] * other[2],
    __this[3] * other[3],
  ];
}
function __int3___operator_modulo(__this, other) {
  return [
    __this[0] % other[0] & 0xffffffff,
    __this[1] % other[1] & 0xffffffff,
    __this[2] % other[2] & 0xffffffff,
  ];
}
function __uint4___operator_plus(__this, other) {
  return [
    ((__this[0] + other[0]) >>> 0) & 0xffffffff,
    ((__this[1] + other[1]) >>> 0) & 0xffffffff,
    ((__this[2] + other[2]) >>> 0) & 0xffffffff,
    ((__this[3] + other[3]) >>> 0) & 0xffffffff,
  ];
}
function __float2___operator_modulo(__this, other) {
  return [__this[0] % other[0], __this[1] % other[1]];
}
function __short___operator_plus(__this, other) {
  return (__this + other) & 0xffff;
}
function __uint2___operator_plus(__this, other) {
  return [
    ((__this[0] + other[0]) >>> 0) & 0xffffffff,
    ((__this[1] + other[1]) >>> 0) & 0xffffffff,
  ];
}
function __uint2___operator_minus(__this, other) {
  return [
    ((__this[0] - other[0]) >>> 0) & 0xffffffff,
    ((__this[1] - other[1]) >>> 0) & 0xffffffff,
  ];
}
