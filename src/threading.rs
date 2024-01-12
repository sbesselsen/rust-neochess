use std::sync::{Arc, RwLock};

pub struct CancelSignal {
    is_stopped: Arc<RwLock<bool>>,
}

impl CancelSignal {
    pub fn is_stopped(&self) -> bool {
        *self.is_stopped.read().expect("need to acquire read lock")
    }
}

pub struct CancelHandle {
    is_stopped: Arc<RwLock<bool>>,
}

impl CancelHandle {
    pub fn new() -> Self {
        CancelHandle {
            is_stopped: Arc::new(RwLock::new(false)),
        }
    }

    pub fn stop(&self) {
        *self.is_stopped.write().expect("need to acquire write lock") = true;
    }

    pub fn signal(&self) -> CancelSignal {
        CancelSignal {
            is_stopped: self.is_stopped.clone(),
        }
    }
}

impl Default for CancelHandle {
    fn default() -> Self {
        Self::new()
    }
}

pub type InterruptableResult<T> = Result<T, InterruptedError>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct InterruptedError;
