use std::{
    collections::{hash_map, HashMap},
    fmt::{Debug, Display, Formatter, Result as FmtResult},
    hash::{Hash, Hasher},
    iter::FusedIterator,
    path::{Path, PathBuf},
    sync::Arc,
};

use lasso::{Rodeo, Spur};

use crate::Result;

#[derive(Debug)]
pub struct SourceManager {
    main_source_key: Spur,
    sources: HashMap<Spur, (Arc<str>, ariadne::Source<Arc<str>>)>,
    rodeo: Rodeo,
}

impl SourceManager {
    pub fn new(main_source_filepath: PathBuf) -> Result<Self> {
        let mut rodeo = Rodeo::new();

        let main_source_filepath = main_source_filepath.canonicalize()?;
        let main_source_filepath_str = main_source_filepath.to_string_lossy();

        let main_source_key = rodeo.get_or_intern(main_source_filepath_str);

        let main_source = Arc::<str>::from(std::fs::read_to_string(&main_source_filepath)?);
        let ariadne_source = ariadne::Source::from(main_source.clone());

        let mut sources = HashMap::new();
        sources.insert(main_source_key, (main_source, ariadne_source));

        Ok(Self {
            main_source_key,
            sources,
            rodeo,
        })
    }

    pub fn get_source(&mut self, filepath: PathBuf) -> Result<Source> {
        let filepath = filepath.canonicalize()?;
        let filepath_str = filepath.to_string_lossy();

        if let Some(key) = self.rodeo.get(&filepath_str) {
            Ok(Source {
                key,
                source_manager: self,
            })
        } else {
            let key = self.rodeo.get_or_intern(filepath_str);

            let source = Arc::<str>::from(std::fs::read_to_string(&filepath)?);
            let ariadne_source = ariadne::Source::from(source.clone());

            self.sources.insert(key, (source, ariadne_source));

            Ok(Source {
                key,
                source_manager: self,
            })
        }
    }

    pub fn get_main_source(&self) -> Source {
        Source {
            key: self.main_source_key,
            source_manager: self,
        }
    }

    pub fn get_cache(&self) -> SourceCache {
        SourceCache { source_manager: self }
    }

    pub fn iter(&self) -> SourceIter {
        SourceIter {
            source_manager: self,
            iter: self.sources.keys(),
        }
    }
}

pub struct SourceCache<'a> {
    source_manager: &'a SourceManager,
}

impl<'a> ariadne::Cache<Source<'a>> for SourceCache<'a> {
    type Storage = Arc<str>;

    fn fetch(&mut self, id: &Source) -> Result<&ariadne::Source<Self::Storage>, Box<dyn Debug + '_>> {
        Ok(&self.source_manager.sources[&id.key].1)
    }

    fn display(&self, id: &Source<'a>) -> Option<Box<dyn Display + 'a>> {
        Some(Box::new(*id))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Source<'a> {
    key: Spur,
    source_manager: &'a SourceManager,
}

impl<'a> Source<'a> {
    pub fn get_filepath_str(&self) -> &'a str {
        self.source_manager.rodeo.resolve(&self.key)
    }

    pub fn get_filepath(&self) -> &'a Path {
        Path::new(self.get_filepath_str())
    }

    pub fn get_contents(&self) -> &'a str {
        &self.source_manager.sources[&self.key].0
    }

    pub fn get_contents_owned(&self) -> Arc<str> {
        self.source_manager.sources[&self.key].0.clone()
    }

    pub(crate) fn get_ariadne_source(&self) -> &'a ariadne::Source<Arc<str>> {
        &self.source_manager.sources[&self.key].1
    }
}

impl<'a> PartialEq for Source<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
    }
}

impl Eq for Source<'_> {}

impl<'a> Hash for Source<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.key.hash(state);
    }
}

impl<'a> Display for Source<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.get_filepath_str())
    }
}

#[derive(Debug, Clone)]
pub struct SourceIter<'a> {
    source_manager: &'a SourceManager,
    iter: hash_map::Keys<'a, Spur, (Arc<str>, ariadne::Source<Arc<str>>)>,
}

impl<'a> Iterator for SourceIter<'a> {
    type Item = Source<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|key| Source {
            key: *key,
            source_manager: self.source_manager,
        })
    }
}

impl<'a> ExactSizeIterator for SourceIter<'a> {
    fn len(&self) -> usize {
        self.iter.len()
    }
}

impl<'a> FusedIterator for SourceIter<'a> {}
