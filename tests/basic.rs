use n0_error::Error;

// #[test]
// fn test_anyhow_compat() -> Result {
//     fn ok() -> anyhow::Result<()> {
//         Ok(())
//     }

//     ok()?;

//     Ok(())
// }

#[derive(Error)]
enum MyError {
    #[display("A failure")]
    A {},
}

#[test]
fn test_whatever() {
    // fn fail() -> Result {
    //     snafu::whatever!("sad face");
    // }

    fn fail_my_error() -> Result<(), MyError> {
        Err(MyError::A {})
    }

    // fn fail_whatever() -> Result {
    //     snafu::whatever!(fail(), "sad");
    //     Ok(())
    // }

    // fn fail_whatever_my_error() -> Result {
    //     snafu::whatever!(fail_my_error(), "sad");
    //     Ok(())
    // }

    // assert!(fail().is_err());
    // assert_eq!(format!("{:?}", fail().unwrap_err()), "sad face");
    // assert_eq!(format!("{}", fail().unwrap_err()), "sad face");
    assert!(fail_my_error().is_err());
    assert_eq!(format!("{}", fail_my_error().unwrap_err()), "A failure");
    assert_eq!(format!("{:#}", fail_my_error().unwrap_err()), "A failure");
    assert_eq!(
        format!("{:?}", fail_my_error().unwrap_err()),
        "A failure  \n"
    );
    assert_eq!(format!("{:#?}", fail_my_error().unwrap_err()), "A");
    // assert!(fail_whatever().is_err());
    // assert!(fail_whatever_my_error().is_err());

    // assert_eq!(
    //     format!("{:?}", fail_whatever().unwrap_err()),
    //     "sad\n  0: sad face"
    // );

    // assert_eq!(format!("{:?}", fail_whatever()), "Err(sad\n  0: sad face)");
}

// #[test]
// fn test_context_none() {
//     fn fail() -> Result {
//         None.context("sad")
//     }

//     assert!(fail().is_err());
// }

// #[test]
// fn test_format_err() {
//     fn fail() -> Result {
//         Err(format_err!("sad: {}", 12))
//     }

//     assert!(fail().is_err());
// }

// #[test]
// fn test_io_err() {
//     fn fail_io() -> std::io::Result<()> {
//         Err(std::io::Error::other("sad IO"))
//     }

//     fn fail_custom() -> Result<(), MyError> {
//         Ok(())
//     }

//     fn fail_outer() -> Result {
//         fail_io().e()?;
//         fail_custom()?;
//         Ok(())
//     }
//     let err = fail_outer().unwrap_err();
//     assert_eq!(err.to_string(), "sad IO");
// }

// #[test]
// fn test_message() {
//     fn fail_box() -> Result<(), impl std::error::Error + Send + Sync + 'static> {
//         Err(Box::new(std::io::Error::other("foo")))
//     }

//     let my_res = fail_box().context("failed");

//     let err = my_res.unwrap_err();
//     let stack = err.stack();
//     assert_eq!(stack.len(), 2);
// }

// #[test]
// fn test_option() {
//     fn fail_opt() -> Option<()> {
//         None
//     }

//     let my_res = fail_opt().context("failed");

//     let err = my_res.unwrap_err();
//     let stack = err.stack();
//     assert_eq!(stack.len(), 2);
// }

// #[test]
// fn test_sources() {
//     let err = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
//     let file_name = "foo.txt";
//     let res: Result<(), _> = Err(err).with_context(|| format!("failed to read {file_name}"));
//     let res: Result<(), _> = res.context("read error");

//     let err = res.err().unwrap();

//     let fmt = format!("{err}");
//     println!("short:\n{fmt}\n");
//     assert_eq!(&fmt, "read error: failed to read foo.txt: file not found");

//     let fmt = format!("{err:#}");
//     println!("alternate:\n{fmt}\n");
//     assert_eq!(
//         &fmt,
//         r#"read error
//   0: failed to read foo.txt
//   1: file not found"#
//     );

//     let fmt = format!("{err:?}");
//     println!("debug:\n{fmt}\n");
// }
