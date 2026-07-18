use crate::infrastructure::config::{LogConfig, LogLevel};
use std::path::Path;
use tracing_subscriber::{fmt, layer::SubscriberExt, util::SubscriberInitExt};

/// Initializes asynchronous file logging and returns a guard that must be kept alive.
pub fn init_logging(config: &LogConfig) -> tracing_appender::non_blocking::WorkerGuard {
    let path = Path::new(&config.path);
    let dir = path.parent().unwrap_or(Path::new("."));
    let file_name = path.file_name().unwrap_or(std::ffi::OsStr::new("pan.log"));

    // We use `never` since logrotate will handle log size management externally
    let file_appender = tracing_appender::rolling::never(dir, file_name);
    let (non_blocking, guard) = tracing_appender::non_blocking(file_appender);

    let level = match config.level {
        LogLevel::Trace => tracing::Level::TRACE,
        LogLevel::Debug => tracing::Level::DEBUG,
        LogLevel::Info => tracing::Level::INFO,
        LogLevel::Warn => tracing::Level::WARN,
        LogLevel::Error => tracing::Level::ERROR,
    };

    tracing_subscriber::registry()
        .with(tracing_subscriber::filter::LevelFilter::from_level(level))
        .with(fmt::layer().with_writer(non_blocking).with_ansi(false)) // Logs to file only, WITHOUT colors!
        .init();

    guard
}
