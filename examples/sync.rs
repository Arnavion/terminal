// The "without sync" line will appear with "a" at 0.1s and "b" at 0.6s.
// In a terminal with sync support, the "with sync" line will appear with "a" and "b" simultaneously at 0.6s.

use std::io::Write;

fn main() {
	let mut stdout = std::io::stdout().lock();

	let mut terminfo = terminal::terminfo::Terminfo::from_env().unwrap();

	let (begin_sync, end_sync) = terminfo.sync().unwrap();
	writeln!(stdout,
		"This terminal supports begin_sync = {:?}, end_sync = {:?}",
		std::ffi::CString::new(begin_sync).unwrap(), std::ffi::CString::new(end_sync).unwrap(),
	).unwrap();

	loop {
		stdout.write_all(b"without sync: ").unwrap();
		stdout.flush().unwrap();
		std::thread::sleep(std::time::Duration::from_millis(100));

		stdout.write_all(b"a").unwrap();
		stdout.flush().unwrap();
		std::thread::sleep(std::time::Duration::from_millis(500));
		stdout.write_all(b"b\n").unwrap();
		stdout.flush().unwrap();

		std::thread::sleep(std::time::Duration::from_secs(1));

		stdout.write_all(b"   with sync: ").unwrap();
		stdout.flush().unwrap();
		std::thread::sleep(std::time::Duration::from_millis(100));

		stdout.write_all(begin_sync).unwrap();
		stdout.write_all(b"a").unwrap();
		stdout.flush().unwrap();
		std::thread::sleep(std::time::Duration::from_millis(500));
		stdout.write_all(b"b\n").unwrap();
		stdout.write_all(end_sync).unwrap();
		stdout.flush().unwrap();

		std::thread::sleep(std::time::Duration::from_secs(1));
	}
}
