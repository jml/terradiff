-- | Repeatedly run an action every so often, storing the result.
--
-- Copyright (c) 2018 Jonathan M. Lange
--
-- This file is part of terradiff.
--
-- terradiff is free software: you can redistribute it and/or modify it
-- under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- terradiff is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with terradiff. If not, see <https://www.gnu.org/licenses/>.
module Terradiff.Poll
  ( Poll
  , runWhilePolling
  , lastResult
  , cancelPoll
  , waitForResult
  ) where

import Protolude

import Control.Concurrent.STM (TVar, newTVarIO, readTVar, writeTVar)
import Data.Time (DiffTime, diffTimeToPicoseconds)

-- | An action that is being run repeatedly.
data Poll a =
  Poll
  { -- | The most recent result of running the action
    lastResultVar :: STM (Maybe a)
  , -- | The asynchronous action running the loop
    loopAsync :: Async ()
  }

-- | Repeatedly run an action in the background, storing the result somewhere.
-- While it is running, perform another action that can get that result (with
-- 'lastResult') if it needs to. When /that/ action is done, stop polling.
runWhilePolling
  :: IO a  -- ^ Action to run repeatedly in the background.
  -> DiffTime  -- ^ Time to wait between starting the background action once
               -- and starting that action again. If the action takes longer
               -- than this interval, just run it again immediately.
  -> (Poll a -> IO b)  -- ^ Action to take while that action is running. Is
                       -- passed a 'Poll' value that can be used to get the
                       -- latest result via 'lastResult', or cancel via
                       -- 'cancelPoll'.
  -> IO b  -- ^ The result of the inner action.
runWhilePolling pollAction interval innerAction = do
  lastResultVar <- newTVarIO Nothing
  withAsync (loop pollAction interval lastResultVar) (innerAction . Poll (readTVar lastResultVar))


-- | Run an action repeatedly forever, storing the latest result in the given 'TVar'.
loop :: IO a -> DiffTime -> TVar (Maybe a) -> IO ()
loop action interval lastResultVar  =
  forever (void (concurrently executeAction waitForInterval))
  where
    executeAction = do
      result <- action
      atomically $ writeTVar lastResultVar (Just result)
    waitForInterval = threadDelay (round (fromIntegral (diffTimeToPicoseconds interval) / 1000000 :: Double))


-- | Get the latest result from a currently running poll. If the action has
-- not run yet, return 'Nothing'.
lastResult :: Poll a -> STM (Maybe a)
lastResult Poll{lastResultVar} = lastResultVar


-- | Get the latest result from a currently running poll. If we don't have a
-- result yet, then wait until we do.
waitForResult :: Poll a -> STM a
waitForResult Poll{lastResultVar} = do
  result <- lastResultVar
  case result of
    Just result' -> pure result'
    Nothing -> retry


-- | Cancel the currently running poll.
cancelPoll :: Poll a -> IO ()
cancelPoll Poll{loopAsync} = cancel loopAsync
