use std::env;
use std::fs;
use std::path::PathBuf;

use sculk::backend::cranelift::CraneliftBackend;
use sculk::backend::Backend;
use sculk::Compiler;

fn main() {
    if let Err(err) = run() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn run() -> Result<(), String> {
    let args = env::args().skip(1).collect::<Vec<_>>();
    if args.is_empty() {
        return Err(usage());
    }

    let mut script_path: Option<String> = None;
    let mut emit_ir = false;
    let mut run_main = true;
    let mut emit_obj: Option<PathBuf> = None;

    let mut index = 0;
    while index < args.len() {
        let arg = &args[index];
        match arg.as_str() {
            "--emit-ir" => {
                emit_ir = true;
            }
            "--no-run" => {
                run_main = false;
            }
            "--run" => {
                run_main = true;
            }
            "--emit-obj" => {
                index += 1;
                let Some(path) = args.get(index) else {
                    return Err("--emit-obj requires a path".to_string());
                };
                emit_obj = Some(PathBuf::from(path));
            }
            _ if arg.starts_with("--emit-obj=") => {
                let Some(path) = arg.strip_prefix("--emit-obj=") else {
                    unreachable!();
                };
                emit_obj = Some(PathBuf::from(path));
            }
            _ if arg.starts_with("--") => {
                return Err(format!("unknown option '{}'", arg));
            }
            _ => {
                if script_path.is_none() {
                    script_path = Some(arg.clone());
                } else {
                    return Err("multiple script paths provided".to_string());
                }
            }
        }
        index += 1;
    }

    let Some(script_path) = script_path else {
        return Err(usage());
    };

    let source = fs::read_to_string(&script_path)
        .map_err(|err| format!("failed to read '{}': {}", script_path, err))?;

    let compiler = Compiler::new();
    let module = compiler
        .compile_source(&source, "main")
        .map_err(|err| format!("compile failed: {}", err))?;

    if emit_ir {
        println!("{}", module);
    }

    let backend = CraneliftBackend::new().map_err(|err| err.to_string())?;

    if let Some(path) = emit_obj {
        let object_bytes = backend
            .generate(&module)
            .map_err(|err| format!("object generation failed: {}", err))?;
        if let Some(parent) = path.parent() {
            if !parent.as_os_str().is_empty() {
                fs::create_dir_all(parent).map_err(|err| {
                    format!(
                        "failed to create output directory '{}': {}",
                        parent.display(),
                        err
                    )
                })?;
            }
        }
        fs::write(&path, object_bytes)
            .map_err(|err| format!("failed to write object file '{}': {}", path.display(), err))?;
        println!("wrote object file {}", path.display());
    }

    if run_main {
        let exit_code = backend
            .run_main(&module)
            .map_err(|err| format!("jit execution failed: {}", err))?;
        println!("program exited with code {}", exit_code);
    }

    Ok(())
}

fn usage() -> String {
    "usage: sculk <file.scl> [--emit-ir] [--emit-obj <path>] [--run|--no-run]".to_string()
}
