use ::frontend::FrontendArgs;

use super::{Driver as DriverTrait, Error};

pub struct Driver {
    args: FrontendArgs,
}

impl Driver {
    pub fn new(args: FrontendArgs) -> Self {
        Self { args }
    }
}

impl DriverTrait for Driver {
    async fn run(&self) -> Result<(), Error> {
        println!("{:#?}", self.args);
        todo!()
    }
}
