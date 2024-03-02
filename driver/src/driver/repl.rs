use super::{Driver as DriverTrait, Result as DriverResult};

pub struct Driver;

impl Driver {
    pub fn new() -> Self {
        Self
    }
}

impl DriverTrait for Driver {
    async fn run(self) -> DriverResult<()> {
        todo!()
    }
}
