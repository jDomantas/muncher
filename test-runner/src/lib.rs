#![cfg(test)]

use std::{cell::RefCell, collections::HashSet, path::{Path, PathBuf}, rc::Rc};

use muncher::Intrinsics;

#[derive(Clone)]
struct Intr {
    stdout: RefCell<String>,
}

impl Intrinsics for Intr {
    fn print(&self, value: &str) {
        self.stdout.borrow_mut().push_str(value);
    }
}

fn do_run(source: &str) -> (String, String) {
    let intr = Rc::new(Intr {
        stdout: Default::default(),
    });
    let stderr = match muncher::eval(source, intr.clone()) {
        Ok(()) => "".to_owned(),
        Err(e) => format!("runtime error at line {}: {}", e.span.start.line, e.msg),
    };
    let stdout: String = intr.stdout.borrow().clone();
    (stdout, stderr)
}

fn check_program_run(
    path: &Path,
    stdout_path: Option<&Path>,
    stderr_path: Option<&Path>,
) {
    println!("running {}", path.display());
    let source = std::fs::read_to_string(path)
        .expect(&format!("failed to read {:?}", path));
    let stdout = match stdout_path {
        Some(path) => std::fs::read_to_string(path)
            .expect(&format!("failed to read {:?}", path))
            .replace("\r\n", "\n"),
        None => "".to_owned(),
    };
    let stderr = match stderr_path {
        Some(path) => std::fs::read_to_string(path)
            .expect(&format!("failed to read {:?}", path))
            .replace("\r\n", "\n"),
        None => "".to_owned(),
    };
    let (actual_out, actual_err) = do_run(&source);
    if actual_err != stderr {
        panic!(
            "program {} gave incorrect error, expected {:?}, got {:?}",
            path.display(),
            stderr,
            actual_err,
        );
    }
    if actual_out != stdout {
        panic!(
            "program {} gave incorrect output, expected {:?}, got {:?}",
            path.display(),
            stdout,
            actual_out,
        );
    }
}

fn file_exists(path: &Path) -> bool {
    match std::fs::metadata(path) {
        Ok(_) => true,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => false,
        Err(e) => panic!("io error: {}", e),
    }
}

fn add_extension(path: &Path, ext: &str) -> PathBuf {
    let mut path = path.to_path_buf().into_os_string();
    path.push(".");
    path.push(ext);
    PathBuf::from(path)
}

#[test]
fn test_cases() {
    let mut seen_files = HashSet::new();
    let mut used_files = HashSet::new();
    for entry in std::fs::read_dir("../programs/test-cases").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        seen_files.insert(path.clone());
        match path.extension() {
            None => continue,
            Some(x) if x != "mnc" => continue,
            Some(_) => {}
        }
        let stdout_path = add_extension(&path, "stdout");
        let stderr_path = add_extension(&path, "stderr");
        let stdout_path = file_exists(&stdout_path).then(|| stdout_path);
        let stderr_path = file_exists(&stderr_path).then(|| stderr_path);
        check_program_run(&path, stdout_path.as_deref(), stderr_path.as_deref());
        used_files.insert(path);
        used_files.extend(stdout_path);
        used_files.extend(stderr_path);
    }
    for file in &seen_files {
        if !used_files.contains(file) {
            panic!("file {} was not used", file.display());
        }
    }
}