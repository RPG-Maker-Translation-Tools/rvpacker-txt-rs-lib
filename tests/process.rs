//! Smoke tests: they run a real game through each mode and fail on panics.
//!
//! Each needs the matching `*_GAME_PATH` environment variable set to a game
//! directory; without it the test errors out on the missing variable.

use rvpacker_txt_rs_lib::{
    EngineType, Mode, Processor, ReadMode, types::FileFlags,
};
use std::{env::var, fs::create_dir_all, path::PathBuf};

type TestResult = Result<(), Box<dyn std::error::Error>>;

fn run(
    env_var: &str,
    data_dir: &str,
    engine: EngineType,
    mode: Mode,
) -> TestResult {
    let game_path = PathBuf::from(var(env_var)?);
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

engine_tests!(read, Mode::Read(ReadMode::Default { force: true }));
engine_tests!(write, Mode::Write);
engine_tests!(purge, Mode::Purge);
