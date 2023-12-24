pub struct Logger<'a>
{
    function: String,
    buffer: &'a mut String,
    force_log: bool,
}
impl<'a> Logger<'a> {
    pub fn new(buffer: &'a mut String, name: &str, force_log: bool,) -> Self {
        Logger {
            function: name.to_owned(),
            buffer,
            force_log,
        }
    }

    pub fn log(&mut self, args: ::std::fmt::Arguments) {
        if self.force_log {
            println!("{}", args)
        }
        else {
            ::std::fmt::write(&mut self.buffer, args).unwrap();
            self.buffer.push_str("\n");
        }
    }
    #[track_caller]
    pub fn error(&mut self, args: ::std::fmt::Arguments) -> ! {
        print!("{}", self.buffer);
        println!(">> {}", self.function);
        self.buffer.clear();
        panic!("{}", args);
    }

    pub fn writer<'s>(&'s mut self) -> Writer<'s,'a> {
        Writer(self, Vec::new())
    }
}
impl Drop for Logger<'_> {
    fn drop(&mut self) {
        if !self.buffer.is_empty() && false {
            print!("{}", self.buffer);
            println!(">> {}", self.function);
        }
    }
}

pub struct Writer<'a,'b>(&'a mut Logger<'b>, Vec<u8>);
impl ::std::io::Write for Writer<'_,'_> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.buffer.push_str(&String::from_utf8_lossy(buf));
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok( () )
    }
}
impl Drop for Writer<'_,'_> {
    fn drop(&mut self) {
        if ! self.0.buffer.ends_with("\n") {
            self.0.buffer.push_str("\n");
        }
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
