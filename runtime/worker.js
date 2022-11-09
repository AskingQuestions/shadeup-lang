function breakpoint(id) {}

self.onmessage = ({
  data: { sharedBuf, width, height, arr, yStart, yEnd, waitBuf },
}) => {
  let img = new Uint8Array(sharedBuf);
  let waits = new Int32Array(waitBuf);

  let half_width = width / 2;

  let half_height = height / 2;

  let max_size = Math.min(half_width, half_height);

  for (let y = yStart; y < yEnd; y++) {
    for (let x = 0; x < width; x++) {
      let i = (y * width + x) * 4;
      let color = [0, 0, 0, 1];

      let dx = x - half_width;
      let dy = y - half_height;
      let dist = Math.sqrt(dx * dx + dy * dy);

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
  self.postMessage({});
};
