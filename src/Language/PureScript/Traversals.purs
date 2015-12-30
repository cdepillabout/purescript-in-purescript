module Language.PureScript.Traversals where

import Prelude

import Data.Either
import Data.Maybe
import Data.Tuple

fstM :: forall f a b c . (Functor f) => (a -> f c) -> Tuple a b -> f (Tuple c b)
fstM f (Tuple a b) = flip Tuple b <$> f a

sndM :: forall f a b c . (Functor f) => (b -> f c) -> Tuple a b -> f (Tuple a c)
sndM f (Tuple a b) = Tuple a <$> f b

-- thirdM :: (Functor f) => (c -> f d) -> (a, b, c) -> f (a, b, d)
-- thirdM f (a, b, c) = (,,) a b <$> f c

pairM :: forall f a b c d . (Applicative f) => (a -> f c) -> (b -> f d) -> Tuple a b -> f (Tuple c d)
pairM f g (Tuple a b)  = Tuple <$> f a <*> g b

maybeM :: forall f a b . (Applicative f) => (a -> f b) -> Maybe a -> f (Maybe b)
maybeM _ Nothing = pure Nothing
maybeM f (Just a) = Just <$> f a

eitherM :: forall f a b c d . (Functor f) => (a -> f c) -> (b -> f d) -> Either a b -> f (Either c d)
eitherM f _ (Left a)  = Left  <$> f a
eitherM _ g (Right b) = Right <$> g b

defS :: forall m st val . (Applicative m) => st -> val -> m (Tuple st val)
defS s val = pure (Tuple s val)


