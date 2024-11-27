use crate::{
    error::RuntimeError,
    gc::{Gc, Trace},
    value::Value,
};
use std::{fmt, ops::Deref, sync::Arc};

#[derive(Clone, Trace)]
pub struct ArcSlice<T> {
    arc: Arc<[T]>,
    start: usize,
}

impl<T> ArcSlice<T> {
    pub fn empty() -> Self {
        Self {
            arc: Arc::from([]),
            start: 0,
        }
    }

    pub fn skip_last(&self) -> impl Iterator<Item = (&T, ArcSlice<T>)> {
        (self.start..(self.arc.len().saturating_sub(1))).map(|i| {
            (
                &self.arc[i],
                ArcSlice {
                    arc: self.arc.clone(),
                    start: i + 1,
                },
            )
        })
    }

    pub fn iter(&self) -> impl Iterator<Item = (&T, ArcSlice<T>)> {
        (self.start..self.arc.len()).map(|i| {
            (
                &self.arc[i],
                ArcSlice {
                    arc: self.arc.clone(),
                    start: i + 1,
                },
            )
        })
    }
}

impl<T: fmt::Debug> fmt::Debug for ArcSlice<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_list().entries(&self.arc[self.start..]).finish()
    }
}

impl<T> From<Vec<T>> for ArcSlice<T> {
    fn from(v: Vec<T>) -> Self {
        Self {
            arc: Arc::from(v),
            start: 0,
        }
    }
}

impl<T> Deref for ArcSlice<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.arc.as_ref()[self.start..]
    }
}

pub fn iter_arc<T>(arc: &Arc<[T]>) -> impl Iterator<Item = (&T, ArcSlice<T>)> {
    (0..arc.len()).map(|i| {
        (
            &arc[i],
            ArcSlice {
                arc: arc.clone(),
                start: i + 1,
            },
        )
    })
}

/// Extension crate for extracting a single value from a Vec
pub trait RequireOne {
    fn require_one(self) -> Result<Gc<Value>, RuntimeError>;
}

impl RequireOne for Vec<Gc<Value>> {
    fn require_one(self) -> Result<Gc<Value>, RuntimeError> {
        let nargs = self.len();
        let [arg] = self
            .try_into()
            .map_err(|_| RuntimeError::wrong_num_of_args(1, nargs))?;
        Ok(arg)
    }
}
