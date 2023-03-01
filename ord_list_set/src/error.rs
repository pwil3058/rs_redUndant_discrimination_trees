// Copyright 2023 Peter Williams <pwil3058@gmail.com> <pwil3058@bigpond.net.au>

use std::{error, fmt};

#[derive(Debug)]
pub enum Error {
    NotInSet,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotInSet => write!(f, "Not a member of the set"),
        }
    }
}

impl error::Error for Error {}
