#![feature(absolute_path)]
#![feature(lazy_cell)]
#![feature(once_cell_try)]

pub mod cli;
pub mod driver;

use clap::Parser as _;

use crate::driver::Error as DriverError;

#[tokio::main]
async fn main() -> Result<(), DriverError> {
    let args = cli::Cli::parse();

    let driver = driver::get_driver(args)?;
    driver.run().await?;

    Ok(())
}
