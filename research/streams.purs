module Research.Streams where
  
data StreamF a s = StreamF s (s -> Tuple s a)

type Stream a = Exists (StreamF a)

-- nats :: Stream Int
nats :: Stream Int
nats = mkExists $ StreamF 0 (\n -> Tuple (n + 1) n)

head :: forall a. Stream a -> a
head = runExists head'
  where
  head' :: forall s. StreamF a s -> a
  head' (StreamF s f) = snd (f s)

tail :: forall a. Stream a -> Stream a
tail =   runExists tail'
  where
    tail' :: forall s. StreamF a s -> Stream a
    tail' (StreamF s f) = mkExists (StreamF (unsafeCoerce (snd (f s))) f)

transactions :: Transaction -> Stream (MonadPerspectivesTransaction Transaction)
transactions t = mkExists $ StreamF t magic
  where
    magic :: Transaction -> Tuple Transaction (MonadPerspectivesTransaction Transaction)
    magic t' = Tuple t (pure t')

x :: Int
x = head $ tail nats
