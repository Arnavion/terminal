#![deny(rust_2018_idioms, warnings)]
#![deny(clippy::all, clippy::pedantic)]
#![allow(
	clippy::let_underscore_drop,
	clippy::missing_errors_doc,
)]

pub trait Terminal: std::io::Write {
	fn width(&self) -> std::io::Result<usize>;
}

impl<W> Terminal for W where W: std::io::Write + std::os::unix::io::AsRawFd {
	fn width(&self) -> std::io::Result<usize> {
		unsafe {
			let fd = std::os::unix::io::AsRawFd::as_raw_fd(self);

			nonzero_or_errno(libc::isatty(fd))?;

			let mut winsize = std::mem::MaybeUninit::uninit();
			zero_or_errno(libc::ioctl(fd, libc::TIOCGWINSZ, winsize.as_mut_ptr()))?;
			let winsize: libc::winsize = winsize.assume_init();

			Ok(winsize.ws_col.into())
		}
	}
}

macro_rules! forward_std_io_write_to_inner {
	($($start:tt)*) => {
		$($start)* {
			fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
				self.inner.write(buf)
			}

			fn write_vectored(&mut self, bufs: &[std::io::IoSlice<'_>]) -> std::io::Result<usize> {
				self.inner.write_vectored(bufs)
			}

			fn flush(&mut self) -> std::io::Result<()> {
				self.inner.flush()
			}

			fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
				self.inner.write_all(buf)
			}
		}
	};
}

macro_rules! forward_terminal_to_inner {
	($($start:tt)*) => {
		$($start)* {
			fn width(&self) -> std::io::Result<usize> {
				self.inner.width()
			}
		}
	};
}

pub struct RawMode<W> where W: std::os::unix::io::AsRawFd {
	inner: W,
	original_termios: libc::termios,
}

impl<W> RawMode<W> where W: std::os::unix::io::AsRawFd {
	pub fn new(inner: W) -> std::io::Result<Self> {
		unsafe {
			let inner_fd = std::os::unix::io::AsRawFd::as_raw_fd(&inner);

			let mut termios = std::mem::MaybeUninit::uninit();
			zero_or_errno(libc::tcgetattr(inner_fd, termios.as_mut_ptr()))?;
			let mut termios = termios.assume_init();

			let original_termios = termios;

			libc::cfmakeraw(&mut termios);

			zero_or_errno(libc::tcsetattr(inner_fd, 0, &termios))?;

			Ok(RawMode {
				inner,
				original_termios,
			})
		}
	}
}

impl<W> Drop for RawMode<W> where W: std::os::unix::io::AsRawFd {
	fn drop(&mut self) {
		unsafe {
			let inner_fd = std::os::unix::io::AsRawFd::as_raw_fd(&self.inner);
			let _ = libc::tcsetattr(inner_fd, 0, &self.original_termios);
		}
	}
}

forward_std_io_write_to_inner! {
	impl<W> std::io::Write for RawMode<W> where W: std::io::Write + std::os::unix::io::AsRawFd
}

forward_terminal_to_inner! {
	impl<W> Terminal for RawMode<W> where W: std::os::unix::io::AsRawFd + Terminal
}

macro_rules! vt_mode {
	(struct $ty:ident => $new:literal / $drop:literal) => {
		pub struct $ty<W> where W: std::io::Write {
			inner: W,
		}

		impl<W> $ty<W> where W: std::io::Write {
			pub fn new(mut inner: W) -> std::io::Result<Self> {
				std::io::Write::write_all(&mut inner, $new)?;
				Ok($ty {
					inner,
				})
			}
		}

		impl<W> Drop for $ty<W> where W: std::io::Write {
			fn drop(&mut self) {
				let _ = std::io::Write::write_all(&mut self.inner, $drop);
				let _ = std::io::Write::flush(&mut self.inner);
			}
		}

		forward_std_io_write_to_inner! {
			impl<W> std::io::Write for $ty<W> where W: std::io::Write
		}

		forward_terminal_to_inner! {
			impl<W> Terminal for $ty<W> where W: Terminal
		}
	};
}

vt_mode! {
	struct AlternateScreen => b"\x1B[?1049h" / b"\x1B[?1049l"
}

vt_mode! {
	struct NoWraparound => b"\x1B[?7l" / b"\x1B[?7h"
}

fn zero_or_errno(result: std::ffi::c_int) -> std::io::Result<()> {
	if result == 0 {
		Ok(())
	}
	else {
		Err(std::io::Error::last_os_error())
	}
}

fn nonzero_or_errno(result: std::ffi::c_int) -> std::io::Result<()> {
	if result == 0 {
		Err(std::io::Error::last_os_error())
	}
	else {
		Ok(())
	}
}
