{-# LANGUAGE DataKinds #-}
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
  GeoDL { geofences :: MapAlg Gid p q q' g
        , timer :: LensAlg p r a
        , cnt :: LensAlg p (State Gid) Gid
        -- alarm optics
        , current :: LensAlg r (State Time) Time
        , alarms :: MapAlg (Gid, Did) r (State Time) (State (Maybe Time)) Time
        -- geofence optics
        -- XXX: this should be a `Getter` instead!
        , region :: LensAlg q (State Region) Region
        , inside :: LensAlg q (State (Set Did)) (Set Did)
        -- initializers
        , initG :: Region -> g }

fromDL :: (Monad p, MonadState g q, MonadState (Maybe g) q', MonadState a r) =>
          GeoDL p q q' r g a -> NetworkView p
fromDL dl = NetworkView {
    pur3 = return
  , bind = (>>=)
  , add = \r -> do
      gid <- fmap runIdentity $ cnt dl $ modify (+1) >> get
      mapAdd (geofences dl) gid (initG dl r)
      return gid
  , remove = \gid -> do
      timer dl $ mapDelBy (alarms dl) ((== gid) . fst)
      mapDelBy (geofences dl) (== gid)
  , a7 = \i pos -> do
      -- updating geofences
      evs <- itr (geofences dl) (\gid -> do
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
        Just (Enter did) -> void $ timer dl $ do
          c <- runIdentity <$> view (current dl)
          mapAdd (alarms dl) (gid, did) (c + 3)
        Just (Exit did) ->
          void $ timer dl $ mapDel (alarms dl) (gid, did)
        _ -> return ()) evs
      -- we're done!
      return $ Prelude.foldr (\(gid, mev) b -> maybe b (\o -> (gid, o) : b) mev) [] evs
  , tick = \t -> do
      set (timer dl `composeM` current dl) t
      ifold (timer dl ~^|->> itr (alarms dl)) (\(i, t2) -> if t >= t2 then [(i, t)] else [])
  , destroy = do
      timer dl $ mapDelBy (alarms dl) (const True)
      mapDelBy (geofences dl) (const True)
}

--------------------
-- Memory Landing --
--------------------

data Network = Network { _n :: Gid
                       , _geofences :: Map Gid Geofence
                       , _alarm :: Alarms } deriving Show

data Geofence = Geofence { _region :: Region
                         , _in :: Set Did } deriving Show

data Alarms = Alarms { _alarms :: Map (Gid, Did) Time
                     , _current :: Time } deriving Show

-- XXX: boilerplate, boilerplate everywhere! use *lens* library to avoid it?
stateDL :: GeoDL (State Network)
                 (State Geofence)
                 (State (Maybe Geofence))
                 (State Alarms)
                 Geofence Alarms
stateDL = GeoDL {
    geofences = MapAlg {
        itr = \f -> StateT (\s -> let gs = Map.toList (_geofences s) in Identity (
          fmap (\(k, v) -> runIdentity $ evalStateT (f k) v) gs,
          s { _geofences = Map.fromList $ fmap (\(k, v) -> (k, execState (f k) v)) gs }))
      , ati = At' $ \gid -> fromLn $
          Lens (\s -> (Map.lookup gid (_geofences s),
                       maybe (s { _geofences = Map.delete gid (_geofences s) })
                             (\a -> s { _geofences = Map.insert gid a (_geofences s) })))
    }
  , timer = fromLn $ Lens (\s -> (_alarm s, \a -> s { _alarm = a }))
  , cnt = fromLn $ Lens (\s -> (_n s, \a -> s { _n = a }))
  , current = fromLn $ Lens (\s -> (_current s, \a -> s { _current = a }))
  , alarms = MapAlg {
      itr = \f -> StateT (\s -> let as = Map.toList (_alarms s) in Identity (
        fmap (\(k, v) -> runIdentity $ evalStateT (f k) v) as,
        s { _alarms = Map.fromList $ fmap (\(k, v) -> (k, execState (f k) v)) as }))
    , ati = At' $ \k -> fromLn $
        Lens (\s -> (Map.lookup k (_alarms s),
                     maybe (s { _alarms = Map.delete k (_alarms s) })
                           (\a -> s { _alarms = Map.insert k a (_alarms s) })))
  }
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
  run conn "CREATE table Region (gid INTEGER PRIMARY KEY, radius INTEGER NOT NULL, x INTEGER NOT NULL, y INTEGER NOT NULL)" []
  run conn "CREATE table Inside (gid INTEGER PRIMARY KEY, did INTEGER NOT NULL)" []
  run conn "CREATE table Alarm (gid INTEGER, did INTEGER, time INTEGER NOT NULL, PRIMARY KEY (gid, did))" []
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
                (State (Maybe Geofence))
                AlarmStateIO
                Geofence Alarms
hdbcDL = GeoDL {
    geofences = MapAlg {
        itr = \f -> do
          conn <- get
          r <- lift $ quickQuery' conn "SELECT * from Region" []
          let regs :: [(Gid, Region)]
              regs = fmap (\[gid, r, x, y] -> (fromSql gid, (fromSql r, (fromSql x, fromSql y)))) r
          xs <- traverse (\(gid, reg) -> StateT (\conn ->
            fmap (\(x, (_, conn)) -> (x, conn)) $
              runStateT (runGeofenceStateIO $ f gid) (gid, conn))) regs
          lift $ commit conn
          return xs
      , ati = At' $ \gid sa -> do
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
    }
  , timer = fmap Identity . runAlarmStateIO
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
  , alarms = MapAlg {
        itr = \f -> AlarmStateIO $ do
          conn <- get
          r <- lift $ quickQuery' conn "SELECT * from Alarm" []
          let as :: [((Gid, Did), Time)]
              as = fmap (\[g, d, t] -> ((fromSql g, fromSql d), fromSql t)) r
          xs <- traverse (\((gid, did), t) -> StateT (\conn -> do
            let (x, t2) = runState (f (gid, did)) t
            run conn "UPDATE Alarm SET time=? where gid=? and did=?" [toSql t2, toSql gid, toSql did]
            return (x, conn))) as
          lift $ commit conn
          return xs
      , ati = At' $ \(gid, did) smt -> AlarmStateIO $ do
          conn <- get
          r <- lift $ quickQuery' conn "SELECT time from Alarm where gid=? and did=?" [toSql gid, toSql did]
          let mt :: Maybe Time
              mt = safeHead $ fmap (\[t] -> fromSql t) r
          let (x, mt2) = runState smt mt
          maybe (lift $ run conn "DELETE from Alarm where gid=? and did=?" [toSql gid, toSql did])
                (\t2 -> lift $ run conn "REPLACE into Alarm (gid, did, time) values (?, ?, ?)"
                  [toSql gid, toSql did, toSql t2])
                mt2
          lift $ commit conn
          return (Identity x)
    }
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
