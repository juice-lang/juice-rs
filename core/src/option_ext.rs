#[allow(clippy::wrong_self_convention)]
pub trait OptionExt {
    type T;

    fn is_none_or(self, func: impl FnOnce(Self::T) -> bool) -> bool;
}

impl<T> OptionExt for Option<T> {
    type T = T;

    #[must_use]
    #[inline]
    fn is_none_or(self, f: impl FnOnce(T) -> bool) -> bool {
        match self {
            None => true,
            Some(x) => f(x),
        }
    }
}
