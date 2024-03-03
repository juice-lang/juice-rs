use ::frontend::{Runner as FrontendRunner, RunnerArgs as FrontendArgs};

use super::{Driver as DriverTrait, Error as DriverError, Result as DriverResult};

pub struct Driver {
    args: FrontendArgs,
}

impl Driver {
    pub fn new(args: FrontendArgs) -> Self {
        Self { args }
    }
}

impl DriverTrait for Driver {
    async fn run(self) -> DriverResult<()> {
        let runner = FrontendRunner::new(self.args);

        if runner.run() {
            Ok(())
        } else {
            Err(DriverError::AlreadyHandled(1))
        }
    }
}
