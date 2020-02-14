module ComposeEngine.Loop
  ( LoopState,
    Input,
    Update,
    Output,
    Render,
    noInput,
    noUpdate,
    noOutput,
    noRender,
    startLoop,
    timedLoop,
  )
where

import ComposeEngine.Types.Loop
import qualified ComposeEngine.Types.Timer as Timer
import qualified Control.Monad.Reader as M
import qualified Control.Monad.State as M
import qualified SDL

type LoopState m s r = M.StateT (Timer.LoopTimer, Either r s) m

noInput :: (Monad m) => Input m ()
noInput = return ()

noUpdate :: Update s r e
noUpdate _ = return ()

noOutput :: (Monad m) => Output m s r e
noOutput _ = return ()

noRender :: (Monad m) => Render m s
noRender = return ()

startLoop :: (M.MonadIO m) => Timer.LoopTimer -> s -> LoopState m s r () -> m r
startLoop timer state loop = do
  eitherResult <- snd <$> M.execStateT loop (timer, Right state)
  case eitherResult of
    Left result -> return result
    Right _ -> fail "returned state instead of result in startLoop"

timedLoop ::
  (M.MonadIO m) =>
  Input m i ->
  Update s r i ->
  Output m s r i ->
  Render m s ->
  LoopState m s r ()
timedLoop input update output render = do
  updateTimer
  updateFrameCounter
  updateCycle input update output
  (timer, state) <- M.get
  case state of
    Left _ -> return ()
    Right state -> do
      M.lift $ M.runReaderT render (timer, state)
      incrementFrame
      timedLoop input update output render

incrementFrame :: (M.Monad m) => LoopState m s r ()
incrementFrame = M.modify increment
  where
    increment (timer, state) =
      let counter = Timer.counter timer
          frames = Timer.frames counter
          counter' = counter {Timer.frames = frames + 1}
          timer' = timer {Timer.counter = counter'}
       in (timer', state)

updateTimer :: (M.MonadIO m) => LoopState m s r ()
updateTimer = do
  (timer, state) <- M.get
  current <- fromIntegral <$> SDL.ticks
  let elapsed = current - Timer.current timer
  let lag = Timer.lag timer + fromIntegral elapsed
  let timer' = timer {Timer.current = current, Timer.elapsed = elapsed, Timer.lag = lag}
  M.put (timer', state)

updateFrameCounter :: (M.Monad m) => LoopState m s r ()
updateFrameCounter = do
  (timer, state) <- M.get
  let counter = Timer.counter timer
  let elapsedMillis = Timer.current timer - Timer.start counter
  let elapsedSeconds = realToFrac elapsedMillis / 1000 :: Float
  let frames = Timer.frames counter
  M.when (elapsedSeconds >= 0.25 && frames > 10) $ do
    let fps = round (realToFrac frames / elapsedSeconds :: Float)
    let counter' = counter {Timer.start = Timer.current timer, Timer.frames = 0, Timer.fps = fps}
    let timer' = timer {Timer.counter = counter'}
    M.put (timer', state)

updateCycle :: (M.Monad m) => Input m i -> Update s r i -> Output m s r i -> LoopState m s r ()
updateCycle input update output = do
  (timer, eitherState) <- M.get
  case eitherState of
    Left _ -> return ()
    Right _ -> M.when (Timer.lag timer > Timer.rate timer) $ do
      inputs <- M.lift input
      let (timer', eitherState') = M.execState (update inputs) (timer, eitherState)
      M.lift $ M.runReaderT (output inputs) (timer', eitherState')
      let lag = Timer.lag timer
          rate = Timer.rate timer
      M.put (timer' {Timer.lag = lag - rate}, eitherState')
      updateCycle input update output
