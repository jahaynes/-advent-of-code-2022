module Common.State

export
data State s a =
    MkState (s -> (s, a))

export
runState : State s a -> s -> (s, a)
runState (MkState run) s = run s

export
Functor (State s) where

    map f (MkState run) = MkState $ \s =>
        let (s', a) = run s in (s', f a)

export
Applicative (State s) where

    pure a = MkState $ \s => (s, a)

    MkState runf <*> MkState runx = MkState $ \s =>
        let (s' , f) = runf s
            (s'', x) = runx s'
        in (s'', f x)

export
Monad (State s) where

    MkState runa >>= f = MkState $ \s =>
        case runa s of
            (s', a) =>
                let MkState r = f a
                in r s'

export
get : State s s
get = MkState $ \s => (s, s)

export
put : s -> State s ()
put x = MkState $ \_ => (x, ())
