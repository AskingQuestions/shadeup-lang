use std::cmp;
use std::f64;
use std::vec::Vec;
use wasm_bindgen::JsCast;
use wasm_bindgen::{prelude::*, Clamped};
use web_sys::{Element, ImageData};

#[wasm_bindgen]
pub struct Inst {
    value: f32,
    mode: u8,
}

#[wasm_bindgen]
pub fn render(width: u32, height: u32, img: &mut [u8]) {
    let instructions = vec![
        Inst {
            value: 10.0,
            mode: 0,
        },
        Inst {
            value: 2.0,
            mode: 1,
        },
        Inst {
            value: 1.0,
            mode: 0,
        },
        Inst {
            value: 0.1,
            mode: 2,
        },
    ];

    // // let document = web_sys::window().unwrap().document().unwrap();
    // // let canvas = document.get_element_by_id("canvas").unwrap();
    // let canvas: web_sys::HtmlCanvasElement = canvas
    //     .dyn_into::<web_sys::HtmlCanvasElement>()
    //     .map_err(|_| ())
    //     .unwrap();

    // let context = canvas
    //     .get_context("2d")
    //     .unwrap()
    //     .unwrap()
    //     .dyn_into::<web_sys::CanvasRenderingContext2d>()
    //     .unwrap();

    // Create image data
    // let mut raw_data: Vec<u8> =
    //     Vec::with_capacity(canvas.width() as usize * canvas.height() as usize * 4);

    let half_width = width / 2;

    let half_height = height / 2;

    let max_size = cmp::min(half_width, half_height);

    for y in 0..height {
        for x in 0..width {
            let i = (y * width + x) as usize * 4;
            let mut color = (0f32, 0f32, 0f32, 1f32);

            let dist = f32::sqrt(
                (x as f32 - half_width as f32).powf(2f32)
                    + (y as f32 - half_height as f32).powf(2f32),
            );

            if dist < max_size as f32 {
                color = (1f32, 0f32, 0f32, 1f32);
            }

            // for inst in &instructions {
            //     match inst.mode {
            //         0 => {
            //             color.0 += inst.value;
            //             color.1 += inst.value;
            //             color.2 += inst.value;
            //         }
            //         1 => {
            //             color.0 /= inst.value;
            //             color.1 /= inst.value;
            //             color.2 /= inst.value;
            //         }
            //         2 => {
            //             color.0 *= inst.value;
            //             color.1 *= inst.value;
            //             color.2 *= inst.value;
            //         }
            //         _ => {}
            //     }
            // }

            img[i + 0] = ((color.0 * 255f32) as u8);
            img[i + 1] = ((color.1 * 255f32) as u8);
            img[i + 2] = ((color.2 * 255f32) as u8);
            img[i + 3] = ((color.3 * 255f32) as u8);
        }
    }

    // let clamped_buf: Clamped<&[u8]> = Clamped(raw_data.as_slice());
    // let image_data_temp =
    //     ImageData::new_with_u8_clamped_array_and_sh(clamped_buf, canvas.width(), canvas.height())
    //         .unwrap();
    // context.put_image_data(&image_data_temp, 0.0, 0.0).unwrap();
}
