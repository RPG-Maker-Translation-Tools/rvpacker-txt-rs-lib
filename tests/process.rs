//! Smoke tests: they run a real game through each mode and fail on panics.
//!
//! Each needs the matching `*_GAME_PATH` environment variable pointed at a game
//! directory. A test whose variable is unset skips instead of failing, so the
//! suite stays green on a machine with no games checked out.

use rvpacker_txt_rs_lib::{EngineType, Mode, Processor, types::FileFlags};
use std::{env::var, fs::create_dir_all, path::PathBuf};

type TestResult = Result<(), Box<dyn std::error::Error>>;

fn run(
    env_var: &str,
    data_dir: &str,
    engine: EngineType,
    mode: Mode,
) -> TestResult {
    let Ok(game_path) = var(env_var) else {
        eprintln!("{env_var} is unset, skipping");
        return Ok(());
    };

    let game_path = PathBuf::from(game_path);
    let source_path = game_path.join(data_dir);
    let translation_path = game_path.join("translation");
    let output_path = game_path.join("output");

    let output = if mode.is_write() {
        create_dir_all(&output_path)?;
        Some(output_path.as_path())
    } else {
        None
    };

    let mut processor = Processor {
        mode,
        file_flags: FileFlags::all(),
        ..Default::default()
    };

    processor.process(engine, source_path, translation_path, output)?;
    Ok(())
}

/// `$name` per engine, one module per mode.
macro_rules! engine_tests {
    ($module:ident, $mode:expr) => {
        mod $module {
            use super::*;

            #[test]
            fn mz() -> TestResult {
                run("MZ_GAME_PATH", "data", EngineType::New, $mode)
            }

            #[test]
            fn mv() -> TestResult {
                run("MV_GAME_PATH", "data", EngineType::New, $mode)
            }

            #[test]
            fn ace() -> TestResult {
                run("VXACE_GAME_PATH", "Data", EngineType::VXAce, $mode)
            }

            #[test]
            fn vx() -> TestResult {
                run("VX_GAME_PATH", "Data", EngineType::VX, $mode)
            }

            #[test]
            fn xp() -> TestResult {
                run("XP_GAME_PATH", "Data", EngineType::XP, $mode)
            }
        }
    };
}

engine_tests!(
    read,
    Mode::Read {
        append: false,
        force: true
    }
);
engine_tests!(write, Mode::Write);
engine_tests!(purge, Mode::Purge);
