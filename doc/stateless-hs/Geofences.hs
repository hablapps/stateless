{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Geofences where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import Data.Map as Map
import Data.Set as Set

import Database.HDBC
import Database.HDBC.Sqlite3

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

data NetworkView p = NetworkView {
    pur3    :: forall x . x -> p x
  , bind    :: forall x y . p x -> (x -> p y) -> p y
  , add     :: Region -> p Gid
  , remove  :: Gid -> p ()
  , a7      :: Did -> Position -> p [(Gid, OutputEvent)]
  , tick    :: Time -> p [((Gid, Did), Time)]
  , destroy :: p ()
}

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
        -- XXX: a map of millions of alarms... this is awful
        , alarms :: LensAlg r (State (Map (Gid, Did) Time)) (Map (Gid, Did) Time)
        -- XXX: I don't have an opinion about this `wentOff` yet. Its mission is
        -- to avoid bringing the whole map to memory.
        , wentOff :: GetterAlg r (Reader [((Gid, Did), Time)]) [((Gid, Did), Time)]
        -- geofence optics
        -- XXX: this should be a `Getter` instead!
        , region :: LensAlg q (State Region) Region
        , inside :: LensAlg q (State (Set Did)) (Set Did)
        -- initializers
        , initG :: Region -> g }

fromDL :: (Monad p, MonadState a r, MonadState g q, MonadState (Maybe g) q') =>
          GeoDL p q q' r g a -> NetworkView p
fromDL dl = NetworkView {
    pur3 = return
  , bind = (>>=)
  , add = \r -> do
      -- XXX: using natural transformations in a beautiful way
      gid <- fmap runIdentity $ cnt dl $ modify (+1) >> get
      set (runAt' (geofence dl) gid) (Just (initG dl r))
      return gid
  , remove = \gid -> do
      modi (alarm dl `composeM` alarms dl) (filterWithKey (\(gid', _) _ -> gid == gid'))
      void $ set (runAt' (geofence dl) gid) Nothing
  , a7 = \i pos -> do
      -- XXX: here we are collecting all the events from geofences. It would be
      -- nice to have a middle-api to rely on, since this is pretty monolithic.
      -- Anyway, we show again that having a complex transformation makes a lot of
      -- sense, so using the natural transformation directly seems to be a nice
      -- pattern.    AlarmStateIO $ lift $ run conn "DELETE from Current" []
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
  , tick = \t -> do
      set (alarm dl `composeM` current dl) t
      runIdentity <$> view' (alarm dl `composeM` wentOff dl)
  , destroy = do
      -- XXX: there must be a better way of destroying existing geofences
      is <- iindex $ geofences dl
      traverse (\i -> set (runAt' (geofence dl) i) Nothing) is
      void $ set (alarm dl `composeM` alarms dl) Map.empty
}

--------------------
-- Memory Landing --
--------------------

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

------------------
-- HDBC Landing --
------------------

loadScheme :: IO ()
loadScheme = do
  conn <- connectSqlite3 "geofences.db"
  run conn "CREATE table Counter (n INTEGER NOT NULL)" []
  run conn "CREATE table Current (time INTEGER NOT NULL)" []
  run conn "CREATE table Region (gid INTEGER NOT NULL, radius INTEGER NOT NULL, x INTEGER NOT NULL, y INTEGER NOT NULL)" []
  run conn "CREATE table Inside (gid INTEGER NOT NULL, did INTEGER NOT NULL)" []
  run conn "CREATE table Alarm (gid INTEGER NOT NULL, did INTEGER NOT NULL, time INTEGER NOT NULL)" []
  run conn "INSERT INTO Counter VALUES (0)" []
  run conn "INSERT INTO Current VALUES (0)" []
  commit conn
  disconnect conn

newtype AlarmStateIO a = AlarmStateIO {
  runAlarmStateIO :: StateT Connection IO a
} deriving (Functor, Applicative, Monad)

instance MonadState Alarms AlarmStateIO where
  get = do
    conn <- AlarmStateIO get
    r <- AlarmStateIO $ lift $ quickQuery' conn "SELECT * from Current" []
    let current = fromSql (head $ head r)
    r <- AlarmStateIO $ lift $ quickQuery' conn "SELECT * from Alarm" []
    let alarms = Map.fromList $ fmap (\[g, d, t] -> ((fromSql g, fromSql d), fromSql t)) r
    AlarmStateIO $ lift $ commit conn
    return $ Alarms alarms current
  put a = do
    conn <- AlarmStateIO get
    r <- AlarmStateIO $ lift $ run conn "UPDATE Current SET time=?" [toSql (_current a)]
    AlarmStateIO $ lift $ run conn "DELETE from Alarm" []
    stmt <- AlarmStateIO $ lift $ prepare conn "INSERT INTO Alarm VALUES (?, ?, ?)"
    AlarmStateIO $ lift $ executeMany stmt (fmap (\((g, d), t) -> [toSql g, toSql d, toSql t]) (Map.toList $ _alarms a))
    AlarmStateIO $ lift $ commit conn

newtype GeofenceStateIO a = GeofenceStateIO {
  runGeofenceStateIO :: StateT (Gid, Connection) IO a
} deriving (Functor, Applicative, Monad)

-- XXX: what if `gid` is not available?
instance MonadState Geofence GeofenceStateIO where
  get = do
    (gid, conn) <- GeofenceStateIO get
    r <- GeofenceStateIO $ lift $ quickQuery' conn "SELECT * from Region where gid=?" [toSql gid]
    let reg :: Region
        reg = head $ fmap (\[_, r, x, y] -> (fromSql r, (fromSql x, fromSql y))) r
    r <- GeofenceStateIO $ lift $ quickQuery' conn "SELECT did from Inside where gid=?" [toSql gid]
    let ins :: Set Did
        ins = Set.fromList $ fmap (\[did] -> fromSql did) r
    GeofenceStateIO $ lift $ commit conn
    return $ Geofence reg ins
  put (Geofence (r, (x, y)) ins) = do
    (gid, conn) <- GeofenceStateIO get
    GeofenceStateIO $ lift $ run conn "DELETE from Inside where gid=?" [toSql gid]
    stmt <- GeofenceStateIO $ lift $ prepare conn "INSERT INTO Inside VALUES (?, ?)"
    GeofenceStateIO $ lift $ executeMany stmt (fmap (\d -> [toSql gid, toSql d]) (Set.toList ins))
    GeofenceStateIO $ lift $ run conn "UPDATE Region SET radius=?, x=?, y=? where gid=?"
      [toSql gid, toSql r, toSql x, toSql y]
    GeofenceStateIO $ lift $ commit conn
    return ()

hdbcDL :: GeoDL (StateT Connection IO)
                GeofenceStateIO
                (State (Maybe Geofence)) -- XXX: I hate this
                AlarmStateIO
                Geofence Alarms
hdbcDL = GeoDL {
    geofence = At' $ \gid sa -> do
      conn <- get
      r <- lift $ quickQuery' conn "SELECT radius, x, y from Region where gid=?" [toSql gid]
      let mreg :: Maybe Region
          mreg = safeHead $ fmap (\[r, x, y] -> (fromSql r, (fromSql x, fromSql y))) r
      r <- lift $ quickQuery' conn "SELECT did from INSIDE where gid=?" [toSql gid]
      let ins :: Set Did
          ins = Set.fromList $ fmap (\[d] -> fromSql d) r
          mg = fmap (\reg -> Geofence reg ins) mreg
          (x, mg2) = runState sa mg
      lift $ run conn "DELETE from Inside where gid=?" [toSql gid]
      lift $ run conn "DELETE from Region where gid=?" [toSql gid]
      case mg2 of
        Nothing -> return ()
        Just (Geofence (r, (x, y)) ins) -> do
          lift $ run conn "INSERT INTO Region VALUES (?, ?, ?, ?)" [toSql gid, toSql r, toSql x, toSql y]
          stmt <- lift $ prepare conn "INSERT INTO Inside VALUES (?, ?)"
          void $ lift $ executeMany stmt $ fmap (\did -> [toSql gid, toSql did]) (Set.toList ins)
      lift $ commit conn
      return (Identity x)
  , geofences = \f -> do
      conn <- get
      r <- lift $ quickQuery' conn "SELECT * from Region" []
      let regs :: [(Gid, Region)]
          regs = fmap (\[gid, r, x, y] -> (fromSql gid, (fromSql r, (fromSql x, fromSql y)))) r
      xs <- traverse (\(gid, reg) -> StateT (\conn ->
        fmap (\(x, (_, conn)) -> (x, conn)) $
          runStateT (runGeofenceStateIO $ f gid) (gid, conn))) regs
      lift $ commit conn
      return xs
  , alarm = fmap Identity . runAlarmStateIO
  , cnt = \sa -> do
      conn <- get
      r <- lift $ quickQuery' conn "SELECT * from Counter" []
      let (x, c) = runState sa (fromSql (head $ head r))
      lift $ run conn "UPDATE Counter SET n=?" [toSql c]
      lift $ commit conn
      return (Identity x)
  -- XXX: alternatively, we could have "get" alarms and return its `current`
  , current = \sa -> do
      conn <- AlarmStateIO $ get
      r <- AlarmStateIO $ lift $ quickQuery' conn "SELECT * from Current" []
      let (x, curr) = runState sa (fromSql (head $ head r))
      AlarmStateIO $ lift $ run conn "UPDATE Current SET time=?" [toSql curr]
      AlarmStateIO $ lift $ commit conn
      return (Identity x)
  , alarms = \sa -> do
      a <- get
      let (x, as) = runState sa (_alarms a)
      put $ a { _alarms = as }
      return (Identity x)
  , wentOff = \ra -> do
      a <- get
      let x = runReader ra (Prelude.filter (\((_, _), t) -> t <= _current a) $ Map.toList $ _alarms a)
      return (Identity x)
  -- XXX: this is too heavy, we're bringing all `did`s as well.
  , region = \sr -> do
      g <- get
      let (x, r2) = runState sr (_region g)
      put (g { _region = r2 })
      return (Identity x)
  , inside = \si -> do
      g <- get
      let (x, i2) = runState si (_in g)
      put (g { _in = i2 })
      return (Identity x)
  , initG = \r -> Geofence r Set.empty }

-- XXX: oh, really?
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a
