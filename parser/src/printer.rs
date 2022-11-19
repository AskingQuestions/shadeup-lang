#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

use crate::ast::Location;

#[derive(Copy, Clone)]
#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub enum AlertLevel {
    Error,
    Warning,
    Info,
}

#[derive(Clone)]
pub struct SpannedAlert {
    pub label: String,

    pub message: String,
    pub level: AlertLevel,
    pub location: Location,

    pub other_location: Option<Location>,
    pub other_message: Option<String>,
}

impl SpannedAlert {
    #[allow(dead_code)]
    pub fn error(label: String, message: String, location: Location) -> Self {
        Self {
            label,
            message,
            level: AlertLevel::Error,
            location,
            other_location: None,
            other_message: None,
        }
    }

    #[allow(dead_code)]
    pub fn warning(label: String, message: String, location: Location) -> Self {
        Self {
            label,
            message,
            level: AlertLevel::Warning,
            location,
            other_location: None,
            other_message: None,
        }
    }

    #[allow(dead_code)]
    pub fn info(label: String, message: String, location: Location) -> Self {
        Self {
            label,
            message,
            level: AlertLevel::Info,
            location,
            other_location: None,
            other_message: None,
        }
    }

    #[allow(dead_code)]
    pub fn error_2(
        label: String,
        message: String,
        location: Location,
        message2: String,
        location2: Location,
    ) -> Self {
        Self {
            label,
            message,
            level: AlertLevel::Error,
            location,
            other_location: Some(location2),
            other_message: Some(message2),
        }
    }

    #[allow(dead_code)]
    pub fn warning_2(
        label: String,
        message: String,
        location: Location,
        message2: String,
        location2: Location,
    ) -> Self {
        Self {
            label,
            message,
            level: AlertLevel::Warning,
            location,
            other_location: Some(location2),
            other_message: Some(message2),
        }
    }

    #[allow(dead_code)]
    pub fn info_2(
        label: String,
        message: String,
        location: Location,
        message2: String,
        location2: Location,
    ) -> Self {
        Self {
            label,
            message,
            level: AlertLevel::Info,
            location,
            other_location: Some(location2),
            other_message: Some(message2),
        }
    }
}
