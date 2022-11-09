// For more comments about what's going on here, check out the `hello_world`
// example.
import * as wasm from "./pkg";

let workerCount = 4;

let workers = [];
let workerBufs = [];

for (let i = 0; i < workerCount; i++) {
  workers.push(new Worker(new URL("./worker.js", import.meta.url)));
  let arrBuf = new SharedArrayBuffer(4);
  let arr = new Int32Array(arrBuf);
  workerBufs.push({ arrBuf, arr });
}

let doWasm = false;

let canvas = document.getElementById("canvas");
let ctx = canvas.getContext("2d");

let canvasData;
let sharedBuf;
let imgArr;

let lastFrame = performance.now();

let fmt = new Intl.NumberFormat("en-US", {
  maximumFractionDigits: 2,
  notation: "compact",
});
let paused = false;

let spaceDown = false;

document.addEventListener("keydown", (e) => {
  if (e.code === "Space") {
    spaceDown = true;
  }
});

document.addEventListener("keyup", (e) => {
  if (e.code === "Space") {
    spaceDown = false;
  }
});

function userInput() {
  return new Promise((res, rej) => {
    paused = true;
    if (spaceDown) {
      setTimeout(() => {
        paused = false;
        res();
      }, 1);
    } else {
      let fn = (e) => {
        if (e.key === " ") {
          document.removeEventListener("keydown", fn);
          paused = false;
          res();
        }
      };

      document.addEventListener("keydown", fn);
    }
  });
}

let pauseTable = new Map();

async function frame() {
  let needsRequest = false;
  async function pause() {
    needsRequest = true;

    await userInput();
  }

  let counter = 0;

  arr[0] = 0;
  arr[1] = 0;

  function pauseFor(id) {
    Atomics.wait(arr, id, 1);
  }

  if (!paused) {
    requestAnimationFrame(frame);
  }
  for (let i = 0; i < canvasData.data.length; i++) {
    canvasData.data[i] = 0;
  }

  if (doWasm) {
    wasm.render(canvas.width, canvas.height, canvasData.data);
  } else {
    let img = canvasData.data;
    let width = canvas.width;
    let height = canvas.height;
    let half_width = width / 2;

    let half_height = height / 2;

    let max_size = Math.min(half_width, half_height);

    for (let y = 0; y < height; y++) {
      for (let x = 0; x < width; x++) {
        let i = (y * width + x) * 4;
        let color = [0, 0, 0, 1];

        let dist = Math.sqrt((x - half_width) ** 2 + (y - half_height) ** 2);

        // pauseFor(0);
        if (dist < max_size) {
          color = [1, 0, 0, 1];
        }

        // if (i % 10000 == 0) {
        //   ctx.putImageData(canvasData, 0, 0);
        //   await pause();
        // }

        img[i + 0] = color[0] * 255;
        img[i + 1] = color[1] * 255;
        img[i + 2] = color[2] * 255;
        img[i + 3] = color[3] * 255;
      }
    }
  }

  ctx.putImageData(canvasData, 0, 0);

  let now = performance.now();

  ctx.font = "20px Arial";
  ctx.fillStyle = "white";
  ctx.fillText(Math.round(now - lastFrame) + " ms", 10, 20);
  ctx.fillText(Math.round(1000 / (now - lastFrame)) + " fps", 10, 40);
  ctx.fillText(fmt.format(canvas.width * canvas.height) + " pixels", 10, 60);

  lastFrame = now;

  spaceDown = false;

  // await pause();

  if (needsRequest) {
    requestAnimationFrame(frame);
  }
}

function setSize() {
  canvas.width = window.innerWidth;
  canvas.height = window.innerHeight;
  canvasData = new ImageData(canvas.width, canvas.height);
  sharedBuf = new SharedArrayBuffer(canvas.width * canvas.height * 4);
  imgArr = new Uint8Array(sharedBuf);
  resetImageData();
  copyImageData();
}

setSize();

window.addEventListener("resize", setSize);

function resetImageData() {
  for (let i = 0; i < imgArr.length; i += 4) {
    imgArr[i] = 0;
    imgArr[i + 1] = 0;
    imgArr[i + 2] = 0;
    imgArr[i + 3] = 255;
  }
}

function copyImageData() {
  for (let i = 0; i < canvasData.data.length; i++) {
    canvasData.data[i] = imgArr[i];
  }

  ctx.putImageData(canvasData, 0, 0);
}

let dispatchesWaiting = 0;

let receivedTime = performance.now();
let dispatchTime = performance.now();
let lastDispatchTime = performance.now();

let lastDraw = performance.now();

let frameDirty = false;

function frame2() {
  requestAnimationFrame(frame2);
  if (frameDirty) {
    copyImageData();
    let now = performance.now();

    ctx.font = "20px Arial";
    ctx.fillStyle = "white";
    ctx.fillText(Math.round(receivedTime - lastDispatchTime) + " ms", 10, 20);
    ctx.fillText(Math.round(1000 / (now - lastDraw)) + " fps", 10, 40);
    lastDraw = now;
    ctx.fillText(fmt.format(canvas.width * canvas.height) + " pixels", 10, 60);
    frameDirty = false;
  }
}

function dispatchDraw() {
  dispatchesWaiting += workerCount;
  lastDispatchTime = dispatchTime;
  dispatchTime = performance.now();

  for (let i = 0; i < workers.length; i++) {
    let yStart = Math.floor((i / workers.length) * canvas.height);
    let yEnd = Math.floor(((i + 1) / workers.length) * canvas.height);
    workers[i].postMessage({
      width: canvas.width,
      height: canvas.height,
      sharedBuf,
      yStart,
      yEnd,
      workerIndex: i,
      waitBuf: workerBufs[i].arrBuf,
    });
  }
}

let readyForDispatch = false;

setInterval(() => {
  if (readyForDispatch) {
    dispatchDraw();
    readyForDispatch = false;
  }
}, 1000 / 60);

for (let worker of workers) {
  worker.onmessage = () => {
    dispatchesWaiting--;

    if (dispatchesWaiting == 0) {
      receivedTime = performance.now();
      readyForDispatch = true;
      frameDirty = true;
    }
  };
}

dispatchDraw();

frame2();
