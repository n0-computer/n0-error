use n0_error::{add_location, Error, StackErrorExt};

#[add_location]
#[derive(Error)]
enum Out {
    #[display("outer")]
    Outer { #[error(from)] source: In },
}

#[add_location]
#[derive(Error)]
enum In {
    #[error(transparent)]
    Wrap { #[error(from)] source: Leaf },
}

#[add_location]
#[derive(Error)]
enum Leaf {
    #[display("leaf")]
    Leaf { #[error(std_err)] source: std::io::Error },
}

#[test]
fn transparent_is_skipped_in_sources_display() {
    let err = std::io::Error::new(std::io::ErrorKind::Other, "io");
    let e = Out::outer(In::wrap(Leaf::leaf(err)));

    // Alternate Display prints one-line sources; transparent variant should be skipped.
    assert_eq!(format!("{e:#}"), "outer: leaf: io");

    // Sources iterator includes transparent variants; first source is transparent wrapper.
    let sources: Vec<_> = e.sources().collect();
    assert_eq!(sources.len(), 3); // transparent wrapper + leaf + std::io::Error
    assert!(sources[0].is_transparent());
}

#[test]
fn boxed_dyn_error_preserves_source() {
    let err = std::io::Error::new(std::io::ErrorKind::Other, "x");
    let any = n0_error::AnyError::from_std(err);
    let boxed = any.into_boxed_dyn_error();
    assert_eq!(boxed.to_string(), "x");
    // Ensure there is no panic when traversing source
    let _ = boxed.source();
}
