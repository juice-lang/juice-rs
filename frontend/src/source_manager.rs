use std::{
    collections::HashMap,
    fmt::{Debug, Display, Formatter, Result as FmtResult},
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
    sync::Arc,
};

use derive_where::derive_where;
use lasso::{Rodeo, Spur};

use crate::{
    source_loc::{SourceLoc, SourceRange},
    Result,
};

pub trait SourceManager: private::SourceManager + Debug + Send + Sync + Sized {
    type Index;
    type Output<T>;

    #[allow(dead_code)]
    fn get_source(&mut self, index: Self::Index) -> Self::Output<Source<Self>>;
    fn get_main_source(&self) -> Source<Self>;
}

mod private {
    use std::{
        fmt::{Debug, Formatter, Result as FmtResult},
        hash::Hash,
    };

    pub trait SourceManager {
        type Key: Debug + Clone + Copy + Eq + Hash + Send + Sync;
        type Storage: AsRef<str>;

        fn get_storage(&self, key: Self::Key) -> &Self::Storage;
        fn display_source(&self, key: Self::Key, f: &mut Formatter<'_>) -> FmtResult;
    }

    pub trait AriadneSourceManager: super::SourceManager {
        fn get_ariadne_source(&self, key: Self::Key) -> &ariadne::Source<Self::Storage>;
    }
}

pub trait AriadneSourceManager: private::AriadneSourceManager {
    fn get_cache(&self) -> SourceCache<Self> {
        SourceCache { source_manager: self }
    }
}

#[derive(Debug)]
pub struct DefaultSourceManager {
    main_source_key: Spur,
    sources: HashMap<Spur, (Arc<str>, ariadne::Source<Arc<str>>)>,
    rodeo: Rodeo,
}

impl DefaultSourceManager {
    pub fn new(main_source_filepath: PathBuf) -> Result<Self> {
        let mut rodeo = Rodeo::new();

        let main_source_filepath = main_source_filepath.canonicalize()?;
        let main_source_filepath_str = main_source_filepath.to_string_lossy();

        let main_source_key = rodeo.get_or_intern(main_source_filepath_str);

        let main_source = Arc::<str>::from(Self::read_to_string(&main_source_filepath)?);
        let ariadne_source = ariadne::Source::from(main_source.clone());

        let mut sources = HashMap::new();
        sources.insert(main_source_key, (main_source, ariadne_source));

        Ok(Self {
            main_source_key,
            sources,
            rodeo,
        })
    }

    fn read_to_string(path: impl AsRef<Path>) -> Result<String> {
        let mut string = std::fs::read_to_string(path)?;

        if !string.ends_with('\n') {
            string.push('\n');
        }

        Ok(string)
    }
}

impl private::SourceManager for DefaultSourceManager {
    type Key = Spur;
    type Storage = Arc<str>;

    fn get_storage(&self, key: Self::Key) -> &Self::Storage {
        &self.sources[&key].0
    }

    fn display_source(&self, key: Self::Key, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.rodeo.resolve(&key))
    }
}

impl SourceManager for DefaultSourceManager {
    type Index = PathBuf;
    type Output<T> = Result<T>;

    fn get_source(&mut self, filepath: PathBuf) -> Result<Source<Self>> {
        let filepath = filepath.canonicalize()?;
        let filepath_str = filepath.to_string_lossy();

        if let Some(key) = self.rodeo.get(&filepath_str) {
            Ok(Source {
                key,
                source_manager: self,
            })
        } else {
            let key = self.rodeo.get_or_intern(filepath_str);

            let source = Arc::<str>::from(Self::read_to_string(&filepath)?);
            let ariadne_source = ariadne::Source::from(source.clone());

            self.sources.insert(key, (source, ariadne_source));

            Ok(Source {
                key,
                source_manager: self,
            })
        }
    }

    fn get_main_source(&self) -> Source<Self> {
        Source {
            key: self.main_source_key,
            source_manager: self,
        }
    }
}

impl private::AriadneSourceManager for DefaultSourceManager {
    fn get_ariadne_source(&self, key: Self::Key) -> &ariadne::Source<Self::Storage> {
        &self.sources[&key].1
    }
}

impl AriadneSourceManager for DefaultSourceManager {}

pub struct SourceCache<'src, M: 'src> {
    source_manager: &'src M,
}

impl<'src, M: 'src + AriadneSourceManager> ariadne::Cache<Source<'src, M>> for SourceCache<'src, M> {
    type Storage = M::Storage;

    fn fetch(&mut self, id: &Source<M>) -> Result<&ariadne::Source<Self::Storage>, Box<dyn Debug + '_>> {
        Ok(self.source_manager.get_ariadne_source(id.key))
    }

    fn display(&self, id: &Source<'src, M>) -> Option<Box<dyn Display + 'src>> {
        Some(Box::new(*id))
    }
}

#[derive_where(Debug, Clone, Copy)]
pub struct Source<'src, M: 'src + SourceManager> {
    key: M::Key,
    source_manager: &'src M,
}

impl<'src, M: 'src + SourceManager> Source<'src, M> {
    pub fn get_contents(&self) -> &'src str {
        self.source_manager.get_storage(self.key).as_ref()
    }

    pub fn get_loc(&self, offset: usize) -> SourceLoc<'src, M> {
        SourceLoc::new(*self, offset)
    }

    pub fn get_range(&self, start: usize, end: usize) -> SourceRange<'src, M> {
        SourceRange::new(*self, start, end)
    }

    pub fn get_eof_range(&self) -> SourceRange<'src, M> {
        let contents = self.get_contents();
        let end = contents.len();
        SourceRange::new(*self, end, end)
    }
}

impl<'src, M: 'src + SourceManager<Storage = Arc<str>>> Source<'src, M> {
    #[allow(dead_code)]
    pub fn get_contents_owned(&self) -> Arc<str> {
        self.source_manager.get_storage(self.key).clone()
    }
}

impl<'src, M: 'src + AriadneSourceManager> Source<'src, M> {
    pub fn get_line_and_column(&self, offset: usize) -> Option<(usize, usize)> {
        self.source_manager
            .get_ariadne_source(self.key)
            .get_offset_line(offset)
            .map(|(_, line, column)| (line, column))
    }
}

impl<'src> Source<'src, DefaultSourceManager> {
    #[allow(dead_code)]
    pub fn get_filepath_str(&self) -> &'src str {
        self.source_manager.rodeo.resolve(&self.key)
    }

    #[allow(dead_code)]
    pub fn get_filepath(&self) -> &'src Path {
        Path::new(self.get_filepath_str())
    }
}

impl<M: SourceManager> PartialEq for Source<'_, M> {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key && std::ptr::eq(self.source_manager, other.source_manager)
    }
}

impl<M: SourceManager> Eq for Source<'_, M> {}

impl<M: SourceManager> Hash for Source<'_, M> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.key.hash(state);
    }
}

impl<M: SourceManager> Display for Source<'_, M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        self.source_manager.display_source(self.key, f)
    }
}

#[cfg(test)]
pub mod test {
    use super::Source;

    #[derive(Debug)]
    pub(crate) struct SourceManager {
        string: &'static str,
    }

    impl SourceManager {
        pub const fn new(string: &'static str) -> Self {
            Self { string }
        }
    }

    impl super::private::SourceManager for SourceManager {
        type Key = ();
        type Storage = &'static str;

        fn get_storage(&self, _: Self::Key) -> &Self::Storage {
            &self.string
        }

        fn display_source(&self, _: Self::Key, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "<static string>")
        }
    }

    impl super::SourceManager for SourceManager {
        type Index = ();
        type Output<T> = T;

        fn get_source(&mut self, _: ()) -> Source<Self> {
            super::Source {
                key: (),
                source_manager: self,
            }
        }

        fn get_main_source(&self) -> super::Source<Self> {
            super::Source {
                key: (),
                source_manager: self,
            }
        }
    }

    impl From<&'static str> for SourceManager {
        fn from(string: &'static str) -> Self {
            Self::new(string)
        }
    }
}
