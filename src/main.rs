// [dependencies]
// winapi = { version = "0.3.9", features = ["consoleapi"] }
use std::{io, process, thread};
use std::io::Write;
use std::time::Duration;
use winapi::um::consoleapi::SetConsoleCtrlHandler;
use interust::{InterustCompiler, Object};

unsafe extern "system" fn ctrl_handler(_: u32) -> i32 {
    io::stdout().write("exit".as_bytes()).expect("");
    process::exit(0); // 프로그램을 종료하고 종료 코드 0으로 반환
}

fn main() {

    // Ctrl+C 신호를 처리하기 위한 플래그
    unsafe {
        SetConsoleCtrlHandler(Some(ctrl_handler), 1);
    }

    let mut interust = InterustCompiler::new();
    // 사용자 입력을 받는 루프
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                if let Some(result) = interust.eval_string(input.as_str()){
                    if let Object::Error(err) = result {
                        println!("RuntimeError : {0}", err);
                    } else {
                        println!("anv : {0}", result);
                    }
                }
            }
            Err(_) => {}
        }

        // 잠시 대기 (optional: 너무 빠른 루프 방지)
        thread::sleep(Duration::from_millis(100));
    }

    println!("Program terminated.");
}

//class Test { private:f64; pub public:f64; pub fn add(&self, i:i64) -> f64 { return self.private + self.public + i; } }