let fpsCanvas = document.getElementById('fps');
let fpsCtx = fpsCanvas.getContext('2d');
let canvas = document.getElementById('canvas');
let gl = canvas.getContext('webgl2');
// let program = gl.createProgram();

function clear() {
	gl.clearColor(0.0, 0.0, 0.0, 0.0);
	gl.clear(gl.COLOR_BUFFER_BIT);
}

function sizeCanvas() {
	canvas.width = window.innerWidth * window.devicePixelRatio;
	canvas.height = window.innerHeight * window.devicePixelRatio;
	fpsCanvas.width = 100 * window.devicePixelRatio;
	fpsCanvas.height = 50 * window.devicePixelRatio;
	fpsCanvas.style.width = '100px';
	fpsCanvas.style.height = '50px';
	gl.viewport(0, 0, canvas.width, canvas.height);
}
sizeCanvas();
window.addEventListener('resize', sizeCanvas);
let now = performance.now();
let gatherSpeed = 1;
let fpsHistogram = new Array(fpsCanvas.width).fill(0);
let fpsHistogramIndex = 0;
let lastFps = 0;
let fps = 0;
let counter = 0;
let frameFunc = () => {};
let initFunc = () => {};
function frameLoop() {
	requestAnimationFrame(frameLoop);

	fps++;
	let then = now;
	now = performance.now();
	let dt = now - then;
	counter += dt;
	if (counter >= 1000) {
		counter -= 1000;
		lastFps = fps;
		fps = 0;
	}

	if (counter > 50) {
		fpsHistogram[fpsHistogramIndex] = 1000 / dt;
		fpsHistogramIndex = (fpsHistogramIndex + 1) % fpsHistogram.length;
	}

	// Draw fps histogram and value
	fpsCtx.clearColor = 'transparent';
	let width = 100 * window.devicePixelRatio;
	let height = 50 * window.devicePixelRatio;
	fpsCtx.clearRect(0, 0, width, height);
	fpsCtx.font = '10px monospace';
	for (let j = 0; j < fpsHistogram.length; j++) {
		let rev = fpsHistogramIndex - j;

		let i = rev < 0 ? fpsHistogram.length + rev : rev;
		let x = (1 - j / fpsHistogram.length) * width;
		let y = height - ((fpsHistogram[i] * height) / 600) * 2;
		fpsCtx.fillRect(x, y, width / fpsHistogram.length, height - y);
	}

	fpsCtx.fillStyle = 'white';
	fpsCtx.font = '22px monospace';
	fpsCtx.fillText(lastFps, 0, 20);
	fpsCtx.lineWidth = 1;
	fpsCtx.strokeStyle = 'black';
	fpsCtx.strokeText(lastFps, 0, 20);

	frameFunc();
}

frameLoop();

function __shadeup_gen_native_shader(code, type) {
	let shader = gl.createShader(type);
	gl.shaderSource(shader, code);
	gl.compileShader(shader);
	if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
		console.error('Shader compile error', gl.getShaderInfoLog(shader));
	}

	return shader;
}

function parse_type(type) {
	let name = '';
	let generics = [];
	let generic = '';
	let depth = 0;
	for (let i = 0; i < type.length; i++) {
		let c = type[i];
		if (c == '<') {
			depth += 1;
			if (depth == 1) {
				continue;
			}
		} else if (c == '>') {
			depth -= 1;
			if (depth == 0) {
				generics.push(parse_type(generic));
				generic = '';
				continue;
			}
		} else if (c == ',' && depth == 1) {
			generics.push(parse_type(generic));
			generic = '';
			continue;
		}

		if (depth == 0) {
			name += c;
		} else {
			generic += c;
		}
	}

	return {
		name: name,
		generics: generics
	};
}

function stringify_type(type) {
	return `${type.name}${
		type.generics.length > 0 ? '<' + type.generics.map(stringify_type).join(',') + '>' : ''
	}`;
}

function translateNumericToGLSL(type) {
	let matches = type.match(/^([A-Za-z]+)(\d)?$/);
	let base = matches[1];
	let size = matches[2] ? parseInt(matches[2]) : 1;
	let type_name = '';

	switch (base) {
		case 'float':
			type_name = 'f';
			break;
		case 'int':
			type_name = 'i';
			break;
		case 'uint':
			type_name = 'i';
			break;
		default:
			return null;
	}

	return `${size}${type_name}`;
}

function translate_type_for_uniform(type) {
	if (type.name == 'array') {
		let inner = translate_type_for_uniform(type.generics[0]);
		return `${inner}[]`;
	} else {
		return translateNumericToGLSL(type);
	}
}

class ShadeupShaderInstance {
	constructor(parentSource, bindings) {
		this.parent = parentSource;
		this.bindings = bindings;
	}

	get(type) {
		let arrSizeChanged = false;
		for (let param of Object.keys(this.parent.params)) {
			let type = parse_type(this.parent.params[param]);
			if (type.name == 'array') {
				if (!this.parent.arraySizes[param]) this.parent.arraySizes[param] = 0;

				if (Array.isArray(this.bindings[param])) {
					if (this.bindings[param].length > this.parent.arraySizes[param]) {
						this.parent.arraySizes[param] = this.bindings[param].length;
						arrSizeChanged = true;
					}
				}
			}
		}

		if (this.parent[type] && !arrSizeChanged) {
			return this.parent[type];
		} else {
			let shader = gl.createShader(type);
			let base = this.parent.source;
			console.log(this.parent.arraySizes);
			for (let param of Object.keys(this.parent.arraySizes)) {
				base = base.replace(`%${param}_size%`, this.parent.arraySizes[param]);
			}

			if (type == gl.VERTEX_SHADER) {
			} else {
				base = base.replace(
					'/*__SHADEUP_TEMPLATE_INSERT_MAIN_BEFORE__*/',
					'out vec4 _i_gl_out_pixel;'
				);
				base = base.replace(
					'/*__SHADEUP_TEMPLATE_INSERT_MAIN_START__*/',
					'ShaderOutput _i_out;\nShaderInput _i_in;\n_i_in.uv = gl_FragCoord.xy;'
				);
				base = base.replace(
					'/*__SHADEUP_TEMPLATE_INSERT_MAIN_END__*/',
					'_i_gl_out_pixel = _i_out.color;'
				);
			}
			let finalSource = `#version 300 es\nprecision mediump float;\n\n${base}`;
			console.log(finalSource);
			gl.shaderSource(shader, finalSource);
			gl.compileShader(shader);
			if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
				console.error('Shader compile error', gl.getShaderInfoLog(shader));
			}

			this.parent[type] = shader;
			return shader;
		}
	}
}

class ShadeupShaderSource {
	constructor(params, code) {
		this.source = code;
		this.params = params;
		this.types = {};
		this.arraySizes = {};
	}

	instance(bindings) {
		return new ShadeupShaderInstance(this, bindings);
	}
}

function __shadeup_gen_shader(params, code) {
	return new ShadeupShaderSource(params, code);
}

let fullscreenVertexShader = __shadeup_gen_native_shader(
	`#version 300 es

precision highp float;

out vec2 texCoord;

void main(void) {
float x = float((gl_VertexID & 1) << 2);
float y = float((gl_VertexID & 2) << 1);
texCoord.x = x * 0.5;
texCoord.y = y * 0.5;
gl_Position = vec4(x - 1.0, y - 1.0, 0, 1);
}`,
	gl.VERTEX_SHADER
);

function setUniform(program, path, val, type) {
	let expandedType = parse_type(type);
	if (expandedType.name == 'array') {
		let innerType = expandedType.generics[0];
		for (let i = 0; i < val.length; i++) {
			setUniform(program, `${path}[${i}]`, val[i], stringify_type(innerType));
		}
		if (path.split('.').length < 2) {
			setUniform(program, `${path}_size`, val.length, 'int');
		}
	} else if (
		expandedType.name.startsWith('float') ||
		expandedType.name.startsWith('int') ||
		expandedType.name.startsWith('uint')
	) {
		let methodName = translateNumericToGLSL(expandedType.name);
		let loc = gl.getUniformLocation(program, path);
		if (!Array.isArray(val)) {
			val = [val];
		}
		methodName = `uniform${methodName}v`;
		gl[methodName](loc, val);
	} else {
		let struct = __shadeup_get_struct(expandedType.name);

		if (struct) {
			for (let field of Object.keys(struct)) {
				setUniform(program, `${path}.${field}`, val[field], struct[field]);
			}
		} else {
			// Non-glsl type, ignore
		}
	}
}

function translateIdentifier(str) {
	return str.replace(/\_\_\_/g, '_ii_').replace(/\_\_/g, '_i_');
}

function __shadeup_dispatch_draw(pixelShaderInst) {
	var program = gl.createProgram();

	let pixelShader = pixelShaderInst.get(gl.FRAGMENT_SHADER);
	gl.attachShader(program, fullscreenVertexShader);
	gl.attachShader(program, pixelShader);
	gl.linkProgram(program);
	gl.useProgram(program);

	for (let k of Object.keys(pixelShaderInst.parent.params)) {
		let paramType = pixelShaderInst.parent.params[k];
		let val = pixelShaderInst.bindings[k];
		setUniform(program, translateIdentifier('__in_' + k), val, paramType);
		// let expandedType = parse_type(paramType);
		// if (expandedType.name == 'array') {
		// 	if (!Array.isArray(val)) {
		// 		val = [val];
		// 	}

		// 	for (let i = 0; i < val.length; i++) {
		// 		let offset = gl.getUniformLocation(
		// 			program,
		// 			'_i_in_' + k.replace('__', '_i_') + '[' + i + ']'
		// 		);
		// 		let method = `uniform${translate_type_for_uniform(expandedType.generics[0])}`;
		// 		gl[method](offset, val[i]);
		// 	}
		// } else {
		// 	let offset = gl.getUniformLocation(program, '_i_in_' + k.replace('__', '_i_'));

		// 	if (!Array.isArray(val)) {
		// 		val = [val];
		// 	}

		// 	let method = `uniform${translate_type_for_uniform(expandedType)}v`;
		// 	gl[method](offset, val);
		// }
	}

	program.createUniform = function (type, name) {
		var location = gl.getUniformLocation(program, name);
		return function (v1, v2, v3, v4) {
			gl['uniform' + type](location, v1, v2, v3, v4);
		};
	};

	gl.drawArrays(gl.TRIANGLE_FAN, 0, 3);
}

function __shadeup_get_struct(name) {
	if (!window.__shadeup_structs) window.__shadeup_structs = {};
	return window.__shadeup_structs[name];
}

function __shadeup_register_struct(name, fields) {
	if (!window.__shadeup_structs) window.__shadeup_structs = {};
	window.__shadeup_structs[name] = fields;
}

function __shadeup_error(err, context) {
	return err || context;

	if (!err.__shadeup_stack) {
		return { stack: [context], error: err, message: err.message };
	} else {
		err.stack.push(context);
		return err;
	}
}

window.addEventListener('message', async function (e) {
	var mainWindow = e.source;
	var result = '';
	try {
		console.log('Got new code', e.data);
		if (e.data.type == 'frame') {
			eval(e.data.source);
			frameFunc = main__shadeup_main;

			// This is hacky, but I'm lazy and it's late
			for (let k of Object.keys(window)) {
				if (k.endsWith('__shadeup___init_file')) {
					let newFunc = window[k];
					if (initFunc.toString() != newFunc.toString()) {
						initFunc = newFunc;
						initFunc();
						console.log('Calling init function', k);
					}
				}
			}
		}
	} catch (e) {
		console.error(e);
		result = 'eval() threw an exception.';
	}
	mainWindow.postMessage(result, e.origin);
});
