use super::{Driver as DriverTrait, Error};

pub struct Driver;

impl Driver {
    pub fn new() -> Self {
        Self
    }
}

impl DriverTrait for Driver {
    async fn run(&self) -> Result<(), Error> {
        todo!()
    }
}
