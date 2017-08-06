{-# LANGUAGE FlexibleContexts #-}
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

newtype IOAlarms a = IOAlarms { runIOAlarms :: IO a }
  deriving (Functor, Applicative, Monad)

instance MonadState Alarms IOAlarms where
  get = do
    conn <- IOAlarms $ connectSqlite3 "geofences.db"
    r <- IOAlarms $ quickQuery' conn "SELECT * from Current" []
    let current = fromSql (head $ head r)
    r <- IOAlarms $ quickQuery' conn "SELECT * from Alarm" []
    let alarms = Map.fromList $ fmap (\[g, d, t] -> ((fromSql g, fromSql d), fromSql t)) r
    IOAlarms $ commit conn >> disconnect conn
    return $ Alarms alarms current
  put a = do
    conn <- IOAlarms $ connectSqlite3 "geofences.db"
    r <- IOAlarms $ run conn "UPDATE Current SET time = ?" [toSql (_current a)]
    IOAlarms $ run conn "DELETE * from Current" []
    stmt <- IOAlarms $ prepare conn "INSERT INTO Alarms VALUES (?, ?, ?)"
    IOAlarms $ executeMany stmt (fmap (\((g, d), t) -> [toSql g, toSql d, toSql t]) (Map.toList $ _alarms a))
    IOAlarms $ commit conn >> disconnect conn

newtype IOGeofence g = IOGeofence { runIOGeofence :: Gid -> IO g }

class IMonadState i a m where
  iget :: i -> m a
  iput :: i -> a -> m ()

hdbcDL :: GeoDL IO (State Geofence) (State (Maybe Geofence)) IOAlarms Geofence Alarms
hdbcDL = GeoDL {
    geofence = At' $ \gid -> \sa -> do
      conn <- connectSqlite3 "geofences.db"
      r <- quickQuery' conn "SELECT * from Region where gid=?" [toSql gid]
      let regs :: Maybe (Gid, Region)
          regs = fmap (\[g, r, x, y] -> (fromSql g, (fromSql r, (fromSql x, fromSql y)))) r
      disconnect conn
      undefined
  , geofences = \f -> do
      conn <- connectSqlite3 "geofences.db"
      r <- quickQuery' conn "SELECT * from Region" []
      let regs :: [(Gid, Region)]
          regs = fmap (\[g, r, x, y] -> (fromSql g, (fromSql r, (fromSql x, fromSql y)))) r
      xs <- traverse (\(g, reg) -> do
        r <- quickQuery' conn "SELECT did from Inside where gid=?" [toSql g]
        let (x, g2) = runState (f g) (Geofence reg (Set.fromList $ fmap (\[d] -> fromSql d) r))
        run conn "DELETE * from Inside where gid=?" [toSql g]
        stmt <- prepare conn "INSERT INTO Inside VALUES (?, ?)"
        executeMany stmt (fmap (\d -> [toSql g, toSql d]) (Set.toList (_in g2)))
        run conn "UPDATE Region SET radius=?, x=?, y=? where gid=?" [toSql (fst $ _region g2), toSql (fst $ snd $ _region g2), toSql (snd $ snd $ _region g2)]
        return x) regs
      disconnect conn
      return xs
  -- XXX: this natural transformation is almost `id`. This just happens because
  -- focus program is self-sufficient.
  , alarm = fmap Identity . runIOAlarms
  , cnt = \sa -> do
      conn <- connectSqlite3 "geofences.db"
      r <- quickQuery' conn "SELECT * from Counter" []
      let (a, s) = runState sa (fromSql (head $ head r))
      run conn "UPDATE Counter SET n=?" [toSql s]
      disconnect conn
      return $ Identity a
  , current = \sa -> do
      a <- get
      let (x, cu) = runState sa (_current a)
      put $ a { _current = cu }
      return (Identity x)
  , alarms = \sa -> do
      a <- get
      let (x, as) = runState sa (_alarms a)
      put $ a { _alarms = as }
      return (Identity x)
  , wentOff = \ra -> do
      conn <- IOAlarms $ connectSqlite3 "geofences.db"
      r <- IOAlarms $ quickQuery' conn "SELECT * from Counter" []
      let curr :: Int; curr = fromSql (head $ head r)
      r <- IOAlarms $ quickQuery' conn "SELECT * from Alarm where time < ?" [toSql curr]
      let alarms :: [((Gid, Did), Time)]; alarms = fmap (\[g, d, t] -> ((fromSql g, fromSql d), fromSql t)) r
      IOAlarms $ commit conn >> disconnect conn >> return (runReaderT ra alarms)
  -- XXX: don't know hot to turn this into a database "program", since I need
  -- the gid to determine which particular geofence I am updating.
  , region = fromLn $ Lens (\s -> (_region s, \a -> s { _region = a }))
  , inside = fromLn $ Lens (\s -> (_in s, \a -> s { _in = a }))
  , initG = \r -> Geofence r Set.empty }
