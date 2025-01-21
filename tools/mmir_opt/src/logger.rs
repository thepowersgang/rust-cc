pub struct Logger<'a>
{
    function: String,
    buffer: ::std::cell::RefCell<&'a mut String>,
    force_log: bool,
}
impl<'a> Logger<'a> {
    pub fn new(buffer: &'a mut String, name: &str, force_log: bool,) -> Self {
        Logger {
            function: name.to_owned(),
            buffer: ::std::cell::RefCell::new(buffer),
            force_log,
        }
    }

    pub fn log(&self, args: ::std::fmt::Arguments) {
        if self.force_log {
            println!("{}", args)
        }
        else {
            let mut b = self.buffer.borrow_mut();
            ::std::fmt::write(&mut *b, args).unwrap();
            b.push_str("\n");
        }
    }
    #[track_caller]
    pub fn error(&self, args: ::std::fmt::Arguments) -> ! {
        let mut b = self.buffer.borrow_mut();
        print!("{}", *b);
        println!(">> {}", self.function);
        b.clear();
        panic!("{}", args);
    }

    pub fn writer<'s>(&'s mut self) -> Writer<'s,'a> {
        Writer(self)
    }
}
impl Drop for Logger<'_> {
    fn drop(&mut self) {
        if !self.buffer.get_mut().is_empty() && false {
            print!("{}", self.buffer.get_mut());
            println!(">> {}", self.function);
        }
    }
}

pub struct Writer<'a,'b>(&'a mut Logger<'b>);
impl ::std::io::Write for Writer<'_,'_> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.buffer.get_mut().push_str(&String::from_utf8_lossy(buf));
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok( () )
    }
}
impl Drop for Writer<'_,'_> {
    fn drop(&mut self) {
        if ! self.0.buffer.get_mut().ends_with("\n") {
            self.0.buffer.get_mut().push_str("\n");
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
