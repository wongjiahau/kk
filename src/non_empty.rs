/// Modified from https://docs.rs/nonempty/0.6.0/src/nonempty/lib.rs.html#32-35
use std::{iter, mem};
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NonEmpty<T> {
    pub head: T,
    pub tail: Vec<T>,
}

impl<T> NonEmpty<T> {
    pub fn new(head: T, tail: Vec<T>) -> NonEmpty<T> {
        NonEmpty { head, tail }
    }
    pub const fn first(&self) -> &T {
        &self.head
    }
    pub fn tail(&self) -> &[T] {
        &self.tail
    }
    pub fn last(&self) -> &T {
        match self.tail.last() {
            None => &self.head,
            Some(e) => e,
        }
    }

    pub fn last_mut(&mut self) -> &mut T {
        match self.tail.last_mut() {
            None => &mut self.head,
            Some(e) => e,
        }
    }

    pub fn len(&self) -> usize {
        self.tail.len() + 1
    }
    pub fn insert(&mut self, index: usize, element: T) {
        let len = self.len();
        assert!(index <= len);

        if index == 0 {
            let head = mem::replace(&mut self.head, element);
            self.tail.insert(0, head);
        } else {
            self.tail.insert(index - 1, element);
        }
    }
    pub fn map<U, F>(self, mut f: F) -> NonEmpty<U>
    where
        F: FnMut(T) -> U,
    {
        NonEmpty {
            head: f(self.head),
            tail: self.tail.into_iter().map(f).collect(),
        }
    }

    pub fn all<F>(&self, mut f: F) -> bool
    where
        F: FnMut(&T) -> bool,
    {
        f(&self.head) && self.tail.iter().all(|element| f(element))
    }

    pub fn any<F>(&self, mut f: F) -> bool
    where
        F: FnMut(&T) -> bool,
    {
        f(&self.head) || self.tail.iter().any(|element| f(element))
    }

    pub fn find_map<F, U>(&self, mut f: F) -> Option<U>
    where
        F: FnMut(&T) -> Option<U>,
    {
        match f(&self.head) {
            Some(u) => Some(u),
            None => self.tail.iter().find_map(f),
        }
    }

    pub fn fold_result<E, O, F>(self, mut f: F) -> Result<NonEmpty<O>, E>
    where
        F: FnMut(T) -> Result<O, E>,
    {
        match f(self.head) {
            Err(error) => Err(error),
            Ok(o) => Ok(NonEmpty {
                head: o,
                tail: self
                    .tail
                    .into_iter()
                    .map(f)
                    .collect::<Result<Vec<O>, E>>()?,
            }),
        }
    }

    pub fn zip<U>(self, other: NonEmpty<U>) -> NonEmpty<(T, U)> {
        NonEmpty {
            head: (self.head, other.head),
            tail: self.tail.into_iter().zip(other.tail.into_iter()).collect(),
        }
    }

    pub fn to_vector(&self) -> Vec<&T> {
        vec![&self.head]
            .into_iter()
            .chain(self.tail.iter())
            .collect()
    }

    pub fn into_vector(self) -> Vec<T> {
        iter::once(self.head).chain(self.tail).collect()
    }

    pub fn unzip<A, B>(non_empty: NonEmpty<(A, B)>) -> (NonEmpty<A>, NonEmpty<B>) {
        let (x, y) = non_empty.head;
        let (xs, bs) = non_empty.tail.into_iter().unzip();
        (
            NonEmpty { head: x, tail: xs },
            NonEmpty { head: y, tail: bs },
        )
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        if index == 0 {
            Some(&self.head)
        } else {
            self.tail.get(index - 1)
        }
    }
}
