pub struct Logger
{
    function: String,
    messages: Vec<String>,
}
impl Logger {
    pub fn new(name: &str) -> Logger {
        Logger {
            function: name.to_owned(),
            messages: Vec::new()
        }
    }

    pub fn log(&mut self, args: ::std::fmt::Arguments) {
        self.messages.push(format!("{}", args));
    }
    #[track_caller]
    pub fn error(&mut self, args: ::std::fmt::Arguments) -> ! {
        for m in &self.messages {
            println!("{}", m);
        }
        println!(">> {}", self.function);
        self.messages.clear();
        panic!("{}", args);
    }

    pub fn writer(&mut self) -> Writer {
        Writer(self, Vec::new())
    }
}
impl Drop for Logger {
    fn drop(&mut self) {
        if !self.messages.is_empty() {
            for m in &self.messages {
                println!("{}", m);
            }
            println!(">> {}", self.function);
            self.messages.clear();
        }
    }
}

pub struct Writer<'a>(&'a mut Logger, Vec<u8>);
impl ::std::io::Write for Writer<'_> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        for &b in buf {
            if b == b'\n' {
                self.0.messages.push(String::from_utf8_lossy(&self.1).into_owned());
                self.1 = Vec::new();
            }
            else {
                self.1.push(b);
            }
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok( () )
    }
}
impl Drop for Writer<'_> {
    fn drop(&mut self) {
        self.0.messages.push(String::from_utf8_lossy(&self.1).into_owned());
        self.1 = Vec::new();
    }
}

macro_rules! log_debug {
    ($logger:expr, $fmt:literal $($args:tt)*) => {{
        crate::logger::Logger::log($logger, format_args!($fmt $($args)*));
    }}
}
macro_rules! log_panic {
    ($logger:expr, $fmt:literal $($args:tt)*) => {{
        crate::logger::Logger::error($logger, format_args!($fmt $($args)*));
    }}
}
