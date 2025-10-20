use std::io;

use self::util::wait_sequential;
use crate::{
    AnyError, Error, Result, StackError, StackErrorExt, StackResultExt, StdResultExt, add_meta,
    anyerr, e, ensure_any, format_err, meta,
};
mod util;

#[test]
#[cfg(feature = "anyhow")]
fn test_anyhow_compat() -> Result {
    fn ok() -> anyhow::Result<()> {
        Ok(())
    }
    ok().map_err(AnyError::from_anyhow)
}

#[add_meta]
#[derive(Error)]
enum MyError {
    #[display("A failure")]
    A {},
}
#[test]
fn test_whatever() {
    let _guard = wait_sequential();

    fn fail() -> Result {
        n0_error::whatever!("sad face");
    }

    fn fail_my_error() -> Result<(), MyError> {
        Err(MyError::A { meta: meta() })
    }

    fn fail_whatever() -> Result {
        n0_error::whatever!(fail(), "sad");
        Ok(())
    }

    fn fail_whatever_my_error() -> Result {
        n0_error::whatever!(fail_my_error(), "sad");
        Ok(())
    }

    n0_error::set_backtrace_enabled(false);
    assert!(fail().is_err());
    assert_eq!(format!("{:?}", fail().unwrap_err()), "sad face");
    assert_eq!(format!("{}", fail().unwrap_err()), "sad face");
    assert!(fail_my_error().is_err());
    assert_eq!(format!("{}", fail_my_error().unwrap_err()), "A failure");
    assert_eq!(format!("{:#}", fail_my_error().unwrap_err()), "A failure");
    assert_eq!(format!("{:?}", fail_my_error().unwrap_err()), "A failure");
    // assert_eq!(
    //     format!("{:#?}", fail_my_error().unwrap_err()),
    //     "A {\n    location: None,\n}"
    // );
    assert!(fail_whatever().is_err());
    assert!(fail_whatever_my_error().is_err());

    assert_eq!(
        format!("{:?}", fail_whatever().unwrap_err()),
        "sad\nCaused by:\n    0: sad face"
    );

    assert_eq!(
        format!("{:?}", fail_whatever()),
        "Err(sad\nCaused by:\n    0: sad face)"
    );

    n0_error::set_backtrace_enabled(true);

    assert!(fail_my_error().is_err());
    assert_eq!(format!("{}", fail_my_error().unwrap_err()), "A failure");
    assert_eq!(format!("{:#}", fail_my_error().unwrap_err()), "A failure");
    assert_eq!(
        format!("{:?}", fail_my_error().unwrap_err()),
        format!("A failure (at src/tests.rs:34:32)")
    );
    //     let expected = r#"A {
    //     location: Some(
    //         Location {
    //             file: "src/tests.rs",
    //             line: 33,
    //             column: 13,
    //         },
    //     ),
    // }"#;
    //     assert_eq!(format!("{:#?}", fail_my_error().unwrap_err()), expected);
}

#[test]
fn test_context_none() {
    fn fail() -> Result {
        None.std_context("sad")
    }

    assert!(fail().is_err());
}

#[test]
fn test_format_err() {
    fn fail() -> Result {
        Err(format_err!("sad: {}", 12))
    }

    assert!(fail().is_err());
}

#[test]
fn test_io_err() {
    fn fail_io() -> std::io::Result<()> {
        Err(std::io::Error::other("sad IO"))
    }

    fn fail_custom() -> Result<(), MyError> {
        Ok(())
    }

    fn fail_outer() -> Result {
        fail_io().e()?;
        fail_custom()?;
        Ok(())
    }
    let err = fail_outer().unwrap_err();
    assert_eq!(err.to_string(), "sad IO");
}

#[test]
fn test_message() {
    fn fail_box() -> Result<(), impl std::error::Error + Send + Sync + 'static> {
        Err(Box::new(std::io::Error::other("foo")))
    }

    let my_res = fail_box().std_context("failed");

    let err = my_res.unwrap_err();
    assert_eq!(format!("{err:#}"), "failed: foo");
    let stack = err.stack();
    assert_eq!(stack.count(), 2);
}

#[test]
fn test_option() {
    fn fail_opt() -> Option<()> {
        None
    }

    let my_res = fail_opt().context("failed");

    let err = my_res.unwrap_err();
    assert_eq!(format!("{err:#}"), "failed: Expected some, found none");
    let stack = err.stack();
    assert_eq!(stack.count(), 2);
}

#[test]
fn test_sources() {
    let _guard = wait_sequential();
    n0_error::set_backtrace_enabled(false);
    let err = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
    let file_name = "foo.txt";
    let res: Result<(), _> = Err(err).with_std_context(|_| format!("failed to read {file_name}"));
    let res: Result<(), AnyError> = res.context("read error");

    let err = res.err().unwrap();

    let fmt = format!("{err}");
    println!("short:\n{fmt}\n");
    assert_eq!(&fmt, "read error");

    let fmt = format!("{err:#}");
    println!("alternate:\n{fmt}\n");
    assert_eq!(&fmt, "read error: failed to read foo.txt: file not found");

    let fmt = format!("{err:?}");
    println!("debug :\n{fmt}\n");
    assert_eq!(
        &fmt,
        r#"read error
Caused by:
    0: failed to read foo.txt
    1: file not found"#
    );

    let fmt = format!("{err:#?}");
    println!("debug alternate:\n{fmt}\n");

    n0_error::set_backtrace_enabled(true);

    let err = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
    let file_name = "foo.txt";
    let res: Result<(), _> = Err(err).with_std_context(|_| format!("failed to read {file_name}"));
    let res: Result<(), _> = res.context("read error");

    let err = res.err().unwrap();

    let fmt = format!("{err}");
    println!("short:\n{fmt}\n");
    assert_eq!(&fmt, "read error");

    let fmt = format!("{err:#}");
    println!("alternate:\n{fmt}\n");
    assert_eq!(&fmt, "read error: failed to read foo.txt: file not found");

    let fmt = format!("{err:?}");
    println!("debug :\n{fmt}\n");
    assert_eq!(
        &fmt,
        r#"read error (at src/tests.rs:195:34)
Caused by:
    0: failed to read foo.txt (at src/tests.rs:194:39)
    1: file not found (at src/tests.rs:194:39)"#
    );
    let fmt = format!("{err:#?}");
    println!("debug alternate:\n{fmt}\n");
}

#[test]
fn test_ensure() {
    let _guard = wait_sequential();
    fn foo() -> Result {
        ensure_any!(false, "sad face");
        Ok(())
    }
    let err = foo().unwrap_err();
    assert_eq!(format!("{err}"), "sad face")
}

#[derive(n0_error::Error)]
struct SomeError;

#[derive(n0_error::Error)]
#[display("fail ({code})")]
struct SomeErrorFields {
    code: u32,
}

#[test]
fn test_structs() {
    let _guard = wait_sequential();

    n0_error::set_backtrace_enabled(false);
    fn fail_some_error() -> Result<(), SomeError> {
        Err(SomeError)
    }

    fn fail_some_error_fields() -> Result<(), SomeErrorFields> {
        Err(SomeErrorFields { code: 22 })
    }

    let res = fail_some_error();
    let err = res.unwrap_err();
    assert_eq!(format!("{err}"), "SomeError");
    let err2 = err.context("bad");
    assert_eq!(format!("{err2:#}"), "bad: SomeError");

    let res = fail_some_error_fields();
    let err = res.unwrap_err();
    assert_eq!(format!("{err}"), "fail (22)");
    let err2 = err.context("bad");
    assert_eq!(format!("{err2:#}"), "bad: fail (22)");
}

#[add_meta]
#[derive(n0_error::Error)]
struct SomeErrorLoc;

#[add_meta]
#[derive(n0_error::Error)]
#[display("fail ({code})")]
struct SomeErrorLocFields {
    code: u32,
}

#[test]
fn test_structs_location() {
    let _guard = wait_sequential();
    n0_error::set_backtrace_enabled(true);
    fn fail_some_error() -> Result<(), SomeErrorLoc> {
        Err(SomeErrorLoc::new())
    }

    fn fail_some_error_fields() -> Result<(), SomeErrorLocFields> {
        Err(SomeErrorLocFields::new(22))
    }

    let res = fail_some_error();
    let err = res.unwrap_err();
    assert_eq!(format!("{err}"), "SomeErrorLoc");
    assert_eq!(format!("{err:?}"), "SomeErrorLoc (at src/tests.rs:282:13)");
    let err2 = err.context("bad");
    assert_eq!(format!("{err2:#}"), "bad: SomeErrorLoc");
    let res = fail_some_error_fields();
    let err = res.unwrap_err();
    assert_eq!(format!("{err}"), "fail (22)");
    let err2 = err.context("bad");
    assert_eq!(format!("{err2:#}"), "bad: fail (22)");
    println!("{err2:?}");
    assert_eq!(
        format!("{err2:?}"),
        r#"bad (at src/tests.rs:298:20)
Caused by:
    0: fail (22) (at src/tests.rs:286:13)"#
    );
}

#[test]
fn test_context() {
    #[add_meta]
    #[derive(n0_error::Error)]
    enum AppError {
        My { source: MyError },
        Baz { source: MyError, count: usize },
    }
    // impl AppError {
    //     fn My() -> fn(MyError) -> Self {
    //         Self::my
    //     }

    //     fn Baz(count: usize) -> impl Fn(MyError) -> Self {
    //         move |source| Self::baz(source, count)
    //     }
    // }
    fn fail_a() -> Result<(), MyError> {
        Err(MyError::A { meta: meta() })
    }
    println!("{:?}", fail_a().std_context("foo").unwrap_err());
    println!("---");
    println!(
        "{:?}",
        fail_a().map_err(|s| e!(AppError::My, s)).unwrap_err()
    );
    println!("---");
    println!(
        "{:?}",
        fail_a()
            .map_err(|source| e!(AppError::Baz { source, count: 32 }))
            .unwrap_err()
    );
    println!("---");
    // println!("{:?}", fail_a().context2(AppError::baz).unwrap_err());
}

#[test]
fn test_any() {
    // fn foo() -> Result {
    //     Err(io::Error::other("foo"))?;
    //     Ok(())
    // }

    // let x = foo();
    let x = anyerr!("foo");
    println!("{x:?}");
    let x = anyerr!(e!(MyError::A));
    println!("{x:?}");
    let x = anyerr!(io::Error::other("foo"));
    println!("{x:?}");
}
