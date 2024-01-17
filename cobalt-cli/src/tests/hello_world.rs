use super::*;

#[test]
fn test_hello_world_aot() {
    let input_path_str = "src/tests/inputs/hello_world.co";
    let output_path_str = "src/tests/outputs/hello_world";

    let input = match clio::Input::new(input_path_str) {
        Ok(p) => p,
        Err(err) => panic!("(clio) failed to load input: {err}"),
    };
    let output = match clio::OutputPath::new(output_path_str) {
        Ok(p) => p,
        Err(err) => panic!("(clio) failed to load output: {err}"),
    };

    let cli = Cli::Aot {
        input: input,
        output: Some(output),
        linked: vec![],
        link_dirs: vec![],
        headers: vec![],
        triple: None,
        emit: OutputType::Executable,
        profile: None,
        continue_if_err: true,
        debug_mangle: false,
        no_default_link: false,
        timings: false,
    };

    if let Err(err) = driver(cli) {
        panic!("failure while compiling file: {err}")
    };

    let command_output = Command::new(output_path_str)
        .output()
        .expect("Failed to execute command");
    let output_str =
        std::str::from_utf8(&command_output.stdout).expect("Output is not valid UTF-8");
    assert_eq!(output_str.trim(), "Hello, world!");
}

#[test]
fn test_hello_world_aot_linked() {
    let input_main_path_str = "src/tests/inputs/hello_world_linked_main.co";
    let input_lib_path_str = "src/tests/inputs/hello_world_linked_lib.co";
    let output_main_path_str = "src/tests/outputs/hello_world_linked";
    let output_lib_path_str = Path::new("src/tests/outputs").join(cobalt_build::libs::format_lib(
        "hello_world_linked",
        cobalt_build::HOST_TRIPLE,
        true,
    ));
    // ---

    let input_lib = match clio::Input::new(input_lib_path_str) {
        Ok(p) => p,
        Err(err) => panic!("(clio) failed to load input: {err}"),
    };
    let output_lib = match clio::OutputPath::new(&output_lib_path_str) {
        Ok(p) => p,
        Err(err) => panic!("(clio) failed to load output: {err}"),
    };

    let cli_lib = Cli::Aot {
        input: input_lib,
        output: Some(output_lib),
        linked: vec![],
        link_dirs: vec![],
        headers: vec![],
        triple: None,
        emit: OutputType::Library,
        profile: None,
        continue_if_err: true,
        debug_mangle: false,
        no_default_link: false,
        timings: false,
    };

    if let Err(err) = driver(cli_lib) {
        panic!("failure while compiling file: {err}")
    };

    // ---

    let input_main = match clio::Input::new(input_main_path_str) {
        Ok(p) => p,
        Err(err) => panic!("(clio) failed to load input: {err}"),
    };
    let output_main = match clio::OutputPath::new(output_main_path_str) {
        Ok(p) => p,
        Err(err) => panic!("(clio) failed to load output: {err}"),
    };

    let cli_main = Cli::Aot {
        input: input_main,
        output: Some(output_main),
        linked: vec!["hello_world_linked".to_string()],
        link_dirs: vec!["src/tests/outputs".into()],
        headers: vec![],
        triple: None,
        emit: OutputType::Executable,
        profile: None,
        continue_if_err: true,
        debug_mangle: false,
        no_default_link: false,
        timings: false,
    };

    if let Err(err) = driver(cli_main) {
        panic!("failure while compiling file: {err}")
    };

    // ---

    let command_output = Command::new(output_main_path_str)
        .output()
        .expect("Failed to execute command");
    let output_str =
        std::str::from_utf8(&command_output.stdout).expect("Output is not valid UTF-8");
    assert_eq!(output_str.trim(), "Hello, world!");
}
