module Common.Monad

-- TODO can be vect
export
replicateM : Monad m => Nat -> m a -> m (List a)
replicateM = go []
    where
    go : (List a) -> Nat -> m a -> m (List a)
    go acc     0   _ = pure $ reverse acc
    go acc (S n) act = act >>= \a => go (a::acc) n act

export
replicateM_ : Monad m => Nat -> m a -> m ()
replicateM_ = go
    where
    go : Nat -> m a -> m ()
    go     0   _ = pure ()
    go (S n) act = act >>= \_ => go n act

export
iterateM : Monad m => (a -> m a) -> Nat -> a -> m a
iterateM f = go
    where
    go : Nat -> a -> m a
    go     0 a = pure a
    go (S n) a = go n =<< f a
