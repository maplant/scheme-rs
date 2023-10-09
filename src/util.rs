use std::{ops::Deref, sync::Arc};

#[derive(Clone, Debug)]
pub struct ArcSlice<T> {
    arc: Arc<[T]>,
    start: usize,
}

impl<T> ArcSlice<T> {
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
