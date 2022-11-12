#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

use crate::ast::Location;
use crate::ast::USizeTuple;
// use colored::{ColoredString, Colorize};

#[derive(Debug, Copy, Clone)]
#[cfg_attr(feature = "wasm", wasm_bindgen)]
pub enum AlertLevel {
    Error,
    Warning,
    Info,
}

impl AlertLevel {
    // pub fn colorize(&self, subject: &str) -> ColoredString {
    //     // match self {
    //     //     AlertLevel::Error => subject.red(),
    //     //     AlertLevel::Warning => subject.yellow(),
    //     //     AlertLevel::Info => subject.green(),
    //     // }
    // }

    pub fn to_string(&self) -> &str {
        match self {
            AlertLevel::Error => "error",
            AlertLevel::Warning => "warning",
            AlertLevel::Info => "info",
        }
    }
}

pub fn get_line_text(source: &str, line: usize) -> &str {
    return source.lines().nth(line - 1).unwrap();
}

pub fn format_pretty_alert(
    source: &str,
    location: Location,
    level: AlertLevel,
    message: &str,
) -> String {
    let USizeTuple(line, column) = location.get_start_line_and_column(source);
    let line_text = get_line_text(source, line);

    let line_number_str = line.to_string();
    let line_number_str_len = line_number_str.len();

    let empty_line = " ".repeat(line_number_str_len) + " | ";
    let line_number = line_number_str + " | ";

    // return format!(
    //     "{}: {}\n{}{} {}:{}:{}\n{}\n{}{}\n{}{}{}",
    //     level.colorize(level.to_string()).bold(),
    //     message.bold(),
    //     " ".repeat((line_number_str_len + 1) / 2),
    //     "-->".cyan(),
    //     "file.shadeup",
    //     line,
    //     column,
    //     empty_line.cyan().bold(),
    //     line_number.cyan().bold(),
    //     line_text,
    //     empty_line.cyan().bold(),
    //     " ".repeat(column),
    //     level.colorize("^".repeat(location.span.1 - location.span.0).as_str())
    // );
    "".to_string()
}

pub fn print_pretty_alert(source: &str, location: Location, level: AlertLevel, message: &str) {
    println!(
        "{}\n",
        format_pretty_alert(source, location, level, message)
    );
}

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
