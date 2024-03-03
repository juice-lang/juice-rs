use crate::source_manager::SourceManager;

pub struct Engine<'a> {
    source_manager: &'a SourceManager,
}

impl<'a> Engine<'a> {
    pub fn new(source_manager: &'a SourceManager) -> Self {
        Self { source_manager }
    }
}
