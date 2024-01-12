use std::{
    cell::RefCell,
    sync::{Arc, RwLock},
};

pub struct CancelSignal {
    is_stopped: Arc<RwLock<bool>>,
    is_stopped_cache: RefCell<bool>,
}

impl CancelSignal {
    pub fn is_stopped(&self) -> bool {
        *self.is_stopped.read().expect("need to acquire read lock")
    }

    pub fn is_stopped_cached(&self) -> bool {
        *self.is_stopped_cache.borrow()
    }

    pub fn update_cache(&self) {
        if !self.is_stopped_cached() {
            *self.is_stopped_cache.borrow_mut() = self.is_stopped()
        }
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
            is_stopped_cache: RefCell::new(
                *self.is_stopped.read().expect("need to acquire read lock"),
            ),
        }
    }
}

impl Default for CancelHandle {
    fn default() -> Self {
        Self::new()
    }
}

pub type InterruptableResult<T> = Result<T, InterruptedError<T>>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct InterruptedError<T>(pub T);

pub trait UnwrapOrInterrupt {
    type Output;
    fn unwrap_or_partial(self) -> Self::Output;
    fn unwrap_with_marker(self) -> (bool, Self::Output);
}

impl<T> UnwrapOrInterrupt for InterruptableResult<T> {
    type Output = T;

    fn unwrap_or_partial(self) -> Self::Output {
        match self {
            Ok(v) => v,
            Err(InterruptedError(v)) => v,
        }
    }

    fn unwrap_with_marker(self) -> (bool, Self::Output) {
        match self {
            Ok(v) => (false, v),
            Err(InterruptedError(v)) => (true, v),
        }
    }
}
