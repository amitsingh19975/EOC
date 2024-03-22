#![allow(dead_code)]

use std::{fmt::Debug, ops::Deref, pin::Pin};

pub(crate) struct Ref<T> {
    inner: Option<Pin<Box<T>>>,
}

impl<T> Ref<T> {
    pub(crate) fn new(inner: T) -> Self {
        Self {
            inner: Some(Box::pin(inner)),
        }
    }

    pub(crate) fn to_imm_ref(&self) -> ImmRef<T> {
        assert_eq!(self.inner.is_some(), true);
        let inner = self.inner.as_ref().unwrap();
        ImmRef::Borrowed(inner.as_ref().get_ref() as *const T)
    }

    pub(crate) fn as_ref(&self) -> &T {
        assert_eq!(self.inner.is_some(), true);
        &*self.inner.as_ref().unwrap()
    }

    pub(crate) fn take_as_imm_ref(&mut self) -> ImmRef<T> {
        ImmRef::Owned(self.inner.take().unwrap())
    }

    pub(crate) fn take(&mut self) -> Ref<T> {
        Ref { inner: self.inner.take() }
    }

    pub(crate) fn as_mut(&mut self) -> Pin<&mut T> {
        assert_eq!(self.inner.is_some(), true);
        let inner = self.inner.as_mut().unwrap().as_mut();
        inner
    }
}

impl<T> Deref for Ref<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T> From<T> for Ref<T> {
    fn from(inner: T) -> Self {
        Self::new(inner)
    }
}

impl<T: Debug> Debug for Ref<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ref").field("inner", &self.inner).finish()
    }
}

#[derive(Clone)]
pub(crate) enum ImmRef<T> {
    Owned(Pin<Box<T>>),
    Borrowed(*const T),
}

impl<T> ImmRef<T> {

    pub(crate) fn as_ref(&self) -> &T {
        match self {
            Self::Owned(inner) => &*inner,
            Self::Borrowed(inner) => {
                assert_ne!(*inner, std::ptr::null());
                unsafe { &**inner }
            },
        }
    }
}

impl<T> Deref for ImmRef<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T: Debug> Debug for ImmRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        assert_ne!(self.as_ref() as *const T, std::ptr::null());
        f.debug_struct("ImmRef").field("inner", &self.as_ref()).finish()
    }
}

impl<T: PartialEq> PartialEq for ImmRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl<T: Eq> Eq for ImmRef<T> {}

unsafe impl<T> Sync for ImmRef<T> where T: Sync {}
unsafe impl<T> Send for ImmRef<T> where T: Send {}

