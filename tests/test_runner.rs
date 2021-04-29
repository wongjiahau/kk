#[test]
fn run_all_tests() {
    use colored::*;
    use insta::assert_snapshot;
    use std::fs;
    use std::process::Command;

    let test_dir = "tests/compiler/";
    let folders = fs::read_dir(test_dir).expect("Failed to read directory");

    let mut successful_test_count = 0;
    for folder in folders {
        let files = fs::read_dir(folder.unwrap().path().to_str().unwrap())
            .expect("Failed to read directory");
        for file in files {
            let file = file.expect("Failed to read entry");
            let filename = file
                .path()
                .to_str()
                .expect("Failed to convert entry to string")
                .to_string();

            if filename.ends_with(".kk") {
                let input_filename = filename;
                print!("{}", input_filename);
                let input = fs::read_to_string(&input_filename).expect("failed to read input file");
                let actual_output = {
                    let output = Command::new("./target/release/kk")
                        .arg("run")
                        .arg(&input_filename)
                        .output()
                        .expect("Failed to run KK CLI");

                    let exit_code = match output.status.code() {
                        Some(code) => code.to_string(),
                        None => "".to_string(),
                    };

                    vec![
                        "============".to_string(),
                        "INPUT".to_string(),
                        "============".to_string(),
                        input.trim().to_string(),
                        "".to_string(),
                        "============".to_string(),
                        "EXIT CODE".to_string(),
                        "============".to_string(),
                        exit_code,
                        "".to_string(),
                        "============".to_string(),
                        "STDOUT".to_string(),
                        "============".to_string(),
                        strip_line_trailing_spaces(
                            String::from_utf8_lossy(&output.stdout.clone()).to_string(),
                        )
                        .trim()
                        .to_string(),
                        "".to_string(),
                        "============".to_string(),
                        "STDERR".to_string(),
                        "============".to_string(),
                        strip_line_trailing_spaces(
                            String::from_utf8_lossy(&output.stderr.clone()).to_string(),
                        )
                        .trim()
                        .to_string(),
                    ]
                    .join("\n")
                };

                let actual_output = strip_line_trailing_spaces(actual_output);

                let stripped_actual_output = strip_line_trailing_spaces(
                    String::from_utf8(
                        strip_ansi_escapes::strip(actual_output.clone())
                            .expect("Failed to strip color"),
                    )
                    .unwrap(),
                );

                assert_snapshot!(input_filename.clone(), stripped_actual_output.trim());
                println!("{}", " PASSED".green());
                successful_test_count += 1;

                // if stripped_actual_output.trim() != expected_output {
                //     let changeset = Changeset::new(&expected_output, &stripped_actual_output, "");
                //     println!("{}", "=".repeat(10));
                //     println!(
                //         "ASSERTION FAILED FOR:\n\n{}",
                //         indent_string(input_filename.to_string(), 4)
                //     );
                //     println!("\n\nINPUT:\n\n{}", indent_string(input, 4));
                //     println!(
                //         "\n\nEXPECTED OUTPUT:\n\n{}",
                //         indent_string(expected_output, 4)
                //     );
                //     println!(
                //         "\n\nACTUAL OUTPUT({}):\n\n{}",
                //         output_filename,
                //         indent_string(actual_output, 4)
                //     );
                //     println!(
                //         "\n\nDIFF (EXPECTED OUTPUT / ACTUAL OUTPUT):\n\n{}",
                //         indent_string(changeset.to_string(), 4)
                //     );
                //     println!("{}", "=".repeat(10));
                //     panic!("ASSERTION FAILED.")
                // } else {
                //     println!("{}", " PASSED".green());
                // }
            }
        }
    }
    println!(" {} passed.", successful_test_count);

    fn strip_line_trailing_spaces(input: String) -> String {
        input
            .split('\n')
            .into_iter()
            .map(|line| line.trim_end().to_string())
            .collect::<Vec<String>>()
            .join("\n")
    }
}
