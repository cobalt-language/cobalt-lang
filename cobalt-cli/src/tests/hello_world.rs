use super::*;

#[test]
fn test_hello_world_aot() {
    let input_path_str = "src/tests/inputs/hello_world.co";
    let output_path_str = "src/tests/outputs/hello_world";

    let input = clio::Input::new(input_path_str);
    assert!(
        input.is_ok(),
        "(clio) failed to load input: {:?}",
        &input.err()
    );
    let output = clio::OutputPath::new(output_path_str);
    assert!(
        output.is_ok(),
        "(clio) failed to load output: {:?}",
        &output.err()
    );

    let cli = Cli::Aot {
        input: input.unwrap(),
        output: Some(output.unwrap()),
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

    let result = driver(cli);
    assert!(
        result.is_ok(),
        "failure while compiling file: {:?}",
        &result.err()
    );

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

    let input_lib = clio::Input::new(input_lib_path_str);
    assert!(
        input_lib.is_ok(),
        "(clio) failed to load input: {:?}",
        &input_lib.unwrap_err()
    );

    let output_lib = clio::OutputPath::new(&output_lib_path_str);
    assert!(
        output_lib.is_ok(),
        "(clio) failed to load output: {:?}",
        &output_lib.unwrap_err()
    );

    let cli_lib = Cli::Aot {
        input: input_lib.unwrap(),
        output: Some(output_lib.unwrap()),
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

    let result_lib = driver(cli_lib);
    assert!(
        result_lib.is_ok(),
        "failure while compiling library file: {:?}",
        &result_lib.err()
    );

    // ---

    let input_main = clio::Input::new(input_main_path_str);
    assert!(
        input_main.is_ok(),
        "(clio) failed to load input: {:?}",
        &input_main.err()
    );
    let output_main = clio::OutputPath::new(output_main_path_str);
    assert!(
        output_main.is_ok(),
        "(clio) failed to load output: {:?}",
        &output_main.err()
    );

    let cli_main = Cli::Aot {
        input: input_main.unwrap(),
        output: Some(output_main.unwrap()),
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

    let result_main = driver(cli_main);
    assert!(
        result_main.is_ok(),
        "failure while compiling main file: {:?}",
        &result_main.err()
    );

    // ---

    let command_output = Command::new(output_main_path_str)
        .output()
        .expect("Failed to execute command");
    let output_str =
        std::str::from_utf8(&command_output.stdout).expect("Output is not valid UTF-8");
    assert_eq!(output_str.trim(), "Hello, world!");
}
