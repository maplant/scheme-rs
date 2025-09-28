//! Input and Output handling

use std::{pin::Pin, sync::Arc};
use tokio::{io::{AsyncRead, BufReader}, sync::Mutex};

use crate::gc::Trace;

pub struct InputPort {
    reader: BufReader<Pin<Box<dyn AsyncRead>>>,
}

impl InputPort {
    pub async fn read_char(&mut self) -> char {
        todo!()
    }

    pub async fn peekn(&mut self, idx: usize) -> char {
        todo!()
    }
}

#[derive(Trace)]
struct PortInner {
    // TODO
}

pub struct Port(pub(crate) Arc<Mutex<PortInner>>);
