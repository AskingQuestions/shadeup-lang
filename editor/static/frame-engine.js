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
	gl.viewport(0, 0, canvas.width, canvas.height);
}
sizeCanvas();
window.addEventListener('resize', sizeCanvas);
let now = performance.now();
let gatherSpeed = 1;
let fpsHistogram = new Array(100).fill(0);
let fpsHistogramIndex = 0;
let lastFps = 0;
let fps = 0;
let counter = 0;
let frameFunc = () => {};
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
	fpsCtx.clearRect(0, 0, 100, 50);
	fpsCtx.fillStyle = 'white';
	fpsCtx.font = '20px monospace';
	fpsCtx.fillText(lastFps, 0, 20);
	fpsCtx.font = '10px monospace';
	for (let j = 0; j < fpsHistogram.length; j++) {
		let rev = fpsHistogramIndex - j;

		let i = rev < 0 ? fpsHistogram.length + rev : rev;
		let x = (1 - j / fpsHistogram.length) * 100;
		let y = 50 - ((fpsHistogram[i] * 50) / 320) * 2;
		fpsCtx.fillRect(x, y, 100 / fpsHistogram.length, 50 - y);
	}

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

class ShadeupShaderInstance {
	constructor(parentSource, bindings) {
		this.parent = parentSource;
		this.bindings = bindings;
	}

	get(type) {
		if (this.parent[type]) {
			return this.parent[type];
		} else {
			let shader = gl.createShader(type);
			let base = this.parent.source;
			if (type == gl.VERTEX_SHADER) {
			} else {
				base = base.replace(
					'/*__SHADEUP_TEMPLATE_INSERT_MAIN_BEFORE__*/',
					'out vec4 _i_gl_out_pixel;'
				);
				base = base.replace(
					'/*__SHADEUP_TEMPLATE_INSERT_MAIN_START__*/',
					'vec4 pixel = vec4(0, 0, 0, 0);'
				);
				base = base.replace('/*__SHADEUP_TEMPLATE_INSERT_MAIN_END__*/', '_i_gl_out_pixel = pixel;');
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

function __shadeup_dispatch_draw(pixelShader) {
	var program = gl.createProgram();

	pixelShader = pixelShader.get(gl.FRAGMENT_SHADER);
	gl.attachShader(program, fullscreenVertexShader);
	gl.attachShader(program, pixelShader);
	gl.linkProgram(program);

	program.createUniform = function (type, name) {
		var location = gl.getUniformLocation(program, name);
		return function (v1, v2, v3, v4) {
			gl['uniform' + type](location, v1, v2, v3, v4);
		};
	};

	gl.useProgram(program);

	gl.drawArrays(gl.TRIANGLE_FAN, 0, 3);
}

function __shadeup_error(err, context) {
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
		}
	} catch (e) {
		console.error(e);
		result = 'eval() threw an exception.';
	}
	mainWindow.postMessage(result, e.origin);
});
