module ComposeEngine.Types.Loop where

import Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           ComposeEngine.Types.Timer

type UpdateState s r = State (LoopTimer, Either r s)
type OutputState m s r = ReaderT (LoopTimer, Either r s) m
type RenderState m s = ReaderT (LoopTimer, s) m

type Input m i = m i
type Update s r i = i -> UpdateState s r ()
type Output m s r i = i -> OutputState m s r ()
type Render m s = RenderState m s ()

getUpdateTimer :: UpdateState s r LoopTimer
getUpdateTimer = gets fst

getUpdateState :: UpdateState s r s
getUpdateState = do
  eitherState <- gets snd
  let Right state = eitherState
  return state

modifyUpdateState :: (s -> s) -> UpdateState s r ()
modifyUpdateState f = modify (\(timer, Right s) -> (timer, Right $ f s))

overUpdateState :: StateT (t, Either r s1) Identity a -> Getting s1 s2 s1 -> ASetter s2 s2 s s1 -> (t, Either r s2) -> (t, Either r s2)
overUpdateState f from to (t, Right s) =
  let (t', Right s') = execState f (t, Right $ view from s)
   in (t', Right $ set to s' s)
overUpdateState _ _ _ s = s

putUpdateTimer :: LoopTimer -> UpdateState s r ()
putUpdateTimer timer = do
  (_, eitherState) <- get
  put (timer, eitherState)

putUpdateState :: s -> UpdateState s r ()
putUpdateState state = do
  (timer, _) <- get
  put (timer, Right state)

putUpdateResult :: r -> UpdateState s r ()
putUpdateResult result = do
  (timer, _) <- get
  put (timer, Left result)

askOutputState :: (Monad m) => OutputState m s r (Either r s)
askOutputState = asks snd

askRenderTimer :: (Monad m) => RenderState m s LoopTimer
askRenderTimer = asks fst

askRenderState :: (Monad m) => RenderState m s s
askRenderState = asks snd
