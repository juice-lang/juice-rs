use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};

use lasso::{Rodeo, Spur};

use crate::Result;

#[derive(Debug, Clone, Copy)]
pub struct SourceHandle<'a> {
    key: Spur,
    source_manager: &'a SourceManager,
}

impl<'a> SourceHandle<'a> {
    pub fn get_filepath(&self) -> &Path {
        Path::new(self.source_manager.rodeo.resolve(&self.key))
    }

    pub fn get_contents(&self) -> Arc<str> {
        self.source_manager.sources[&self.key].clone()
    }
}

impl<'a> PartialEq for SourceHandle<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
    }
}

impl Eq for SourceHandle<'_> {}

#[derive(Debug)]
pub struct SourceManager {
    main_source_key: Spur,
    sources: HashMap<Spur, Arc<str>>,
    rodeo: Rodeo,
}

impl SourceManager {
    pub fn new(main_source_filepath: PathBuf) -> Result<Self> {
        let mut rodeo = Rodeo::new();

        let main_source_filepath = main_source_filepath.canonicalize()?;
        let main_source_filepath_str = main_source_filepath.to_string_lossy();

        let main_source = Arc::from(std::fs::read_to_string(&main_source_filepath)?);

        let main_source_key = rodeo.get_or_intern(main_source_filepath_str);

        let mut sources = HashMap::new();
        sources.insert(main_source_key, main_source);

        Ok(Self {
            main_source_key,
            sources,
            rodeo,
        })
    }

    pub fn get_source(&mut self, filepath: PathBuf) -> Result<SourceHandle> {
        let filepath = filepath.canonicalize()?;
        let filepath_str = filepath.to_string_lossy();

        if let Some(key) = self.rodeo.get(&filepath_str) {
            Ok(SourceHandle {
                key,
                source_manager: self,
            })
        } else {
            let source = Arc::from(std::fs::read_to_string(&filepath)?);

            let key = self.rodeo.get_or_intern(filepath_str);

            self.sources.insert(key, source);

            Ok(SourceHandle {
                key,
                source_manager: self,
            })
        }
    }

    pub fn get_main_source(&self) -> SourceHandle {
        SourceHandle {
            key: self.main_source_key,
            source_manager: self,
        }
    }
}
