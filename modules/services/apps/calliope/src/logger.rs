use std::fs::OpenOptions;
use std::io::Write;
use std::sync::Mutex;

static LOG_MUTEX: Mutex<()> = Mutex::new(());

pub fn log_msg(msg: &str) {
    let _guard = LOG_MUTEX.lock().unwrap_or_else(|e| e.into_inner());
    if let Ok(mut file) = OpenOptions::new()
        .create(true)
        .append(true)
        .open("calliope.log")
    {
        let _ = writeln!(file, "{}", msg);
        let _ = file.flush();
    }
}

#[macro_export]
macro_rules! log_info {
    ($($arg:tt)*) => {
        $crate::logger::log_msg(&format!($($arg)*));
    };
}
