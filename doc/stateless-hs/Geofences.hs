{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Geofences where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import Data.Map as Map
import Data.Set as Set

import Stateless

---------------------
-- Aliases & Utils --
---------------------

type Did = Int -- device id
type Gid = Int -- geofence id
type Position = (Int, Int)
type Time = Int
type Region = (Int, Position)

data OutputEvent = Enter Did | Exit Did deriving Show

posInRegion :: Position -> Region -> Bool
posInRegion (x', y') (r, (x, y)) = (abs (x - x') <= r) && (abs (y - y') <= r)

-----------
-- Views --
-----------

data NetworkViewF p x where
  Pure :: x -> NetworkViewF p x
  Bind :: p x -> (x -> p y) -> NetworkViewF p y
  Add :: Region -> NetworkViewF p Gid
  Remove :: Gid -> NetworkViewF p ()
  At :: Did -> Position -> NetworkViewF p [(Gid, OutputEvent)]
  Tick :: Time -> NetworkViewF p [((Gid, Did), Time)]
  Destroy :: NetworkViewF p ()

type NetworkView p = forall x. NetworkViewF p x -> p x

----------------
-- Data Layer --
----------------

data GeoDL p q q' r g a =
  GeoDL { geofence :: At' Gid p q' g
        , geofences :: ITraversalAlg Gid p q g
        , alarm :: LensAlg p r a
        , cnt :: LensAlg p (State Gid) Gid
        -- alarm optics
        , current :: LensAlg r (State Time) Time
        , alarms :: LensAlg r (State (Map (Gid, Did) Time)) (Map (Gid, Did) Time)
        -- XXX: I don't have an opinion about this `wentOff` yet. Its mission is
        -- to avoid bringing the whole map to memory.
        , wentOff :: GetterAlg r (Reader [((Gid, Did), Time)]) [((Gid, Did), Time)]
        -- geofence optics
        , region :: LensAlg q (State Region) Region
        , inside :: LensAlg q (State (Set Did)) (Set Did)
        -- initializers
        , initG :: Region -> g }

fromDL :: (Monad p, MonadState a r, MonadState g q, MonadState (Maybe g) q') =>
          GeoDL p q q' r g a -> NetworkView p
fromDL dl nvf = case nvf of
  Pure x -> return x
  Bind px f -> px >>= f
  Add r -> do
    -- XXX: using natural transformations in a beautiful way
    gid <- fmap runIdentity $ cnt dl $ modify (+1) >> get
    set (runAt' (geofence dl) gid) (Just (initG dl r))
    return gid
  Remove gid -> do
    modi (alarm dl `composeM` alarms dl) (filterWithKey (\(gid', _) _ -> gid == gid'))
    void $ set (runAt' (geofence dl) gid) Nothing
  At i pos -> do
    -- XXX: here we are collecting all the events from geofences. It would be
    -- nice to have a middle-api to rely on, since this is pretty monolithic.
    -- Anyway, we show again that having a complex transformation makes a lot of
    -- sense, so using the natural transformation directly seems to be a nice
    -- pattern.
    evs <- geofences dl (\gid -> do
      -- updating regions
      reg <- runIdentity <$> view (region dl)
      cnd1 <- (Set.member i . runIdentity) <$> view (inside dl)
      let cnd2 = posInRegion pos reg
      case (cnd1, cnd2) of
        (True, False) -> do
          modi (inside dl) (Set.delete i)
          return (gid, Just $ Exit i)
        (False, True) -> do
          modi (inside dl) (Set.insert i)
          return (gid, Just $ Enter i)
        _ -> return (gid, Nothing))
    -- updating alarms
    traverse (\(gid, mev) -> case mev of
      -- XXX: this manifests two different approaches to relate different
      -- contexts: on the one hand (Enter) it is shown how to compose the inner
      -- program and then pass it trough the natural transformation. On the
      -- other hand (Exit), there's a composition of optics which is achieved
      -- before invoking `modi`.
      Just (Enter did) -> void $ alarm dl $ do
        c <- runIdentity <$> view (current dl)
        modi (alarms dl) (Map.insert (gid, did) (c + 3))
      Just (Exit did) ->
        void $ modi (alarm dl `composeM` alarms dl) (Map.delete (gid, did))
      _ -> return ()) evs
    -- we're done!
    return $ Prelude.foldr (\(gid, mev) b -> maybe b (\o -> (gid, o) : b) mev) [] evs
  Tick t -> do
    set (alarm dl `composeM` current dl) t
    runIdentity <$> view' (alarm dl `composeM` wentOff dl)
  Destroy -> do
    -- XXX: there must be a better way of destroying existing geofences
    is <- iindex $ geofences dl
    traverse (\i -> set (runAt' (geofence dl) i) Nothing) is
    void $ set (alarm dl `composeM` alarms dl) Map.empty

-------------
-- Landing --
-------------

data Network = Network { _n :: Gid
                       , _geofences :: Map Gid Geofence
                       , _alarm :: Alarms } deriving Show

data Geofence = Geofence { _region :: Region
                         , _in :: Set Int } deriving Show

data Alarms = Alarms { _alarms :: Map (Gid, Did) Time
                     , _current :: Time } deriving Show

-- XXX: boilerplate, boilerplate everywhere! use *lens* to avoid it?
stateDL :: GeoDL (State Network)
                 (State Geofence)
                 (State (Maybe Geofence))
                 (State Alarms)
                 Geofence Alarms
stateDL = GeoDL {
    geofence = At' $ \gid -> fromLn $
      Lens (\s -> (Map.lookup gid (_geofences s),
                   maybe (s { _geofences = Map.delete gid (_geofences s) })
                         (\a -> s { _geofences = Map.insert gid a (_geofences s) })))
  , geofences = \f -> StateT (\s -> let gs = Map.toList (_geofences s) in Identity (
      fmap (\(k, v) -> runIdentity $ evalStateT (f k) v) gs,
      s { _geofences = Map.fromList $ fmap (\(k, v) -> (k, execState (f k) v)) gs }))
  , alarm = fromLn $ Lens (\s -> (_alarm s, \a -> s { _alarm = a }))
  , cnt = fromLn $ Lens (\s -> (_n s, \a -> s { _n = a }))
  , current = fromLn $ Lens (\s -> (_current s, \a -> s { _current = a }))
  , alarms = fromLn $ Lens (\s -> (_alarms s, \a -> s { _alarms = a }))
  , wentOff = \ra -> StateT (\s -> Identity (runReaderT ra (
      Prelude.filter (\((_, _), t) -> t <= _current s) $ Map.toList $ _alarms s),
      s))
  , region = fromLn $ Lens (\s -> (_region s, \a -> s { _region = a }))
  , inside = fromLn $ Lens (\s -> (_in s, \a -> s { _in = a }))
  , initG = \r -> Geofence r Set.empty }
