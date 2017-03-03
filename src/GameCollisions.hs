-- | A very rudimentary collision system.
--
-- It compares every pair of objects, trying to determine if there is a
-- collision between the two of them.
--
-- NOTE: In order to minimize the number of comparisons, only moving objects
-- are tested (against every game object). That's only 2 objects right now
-- (making it almost linear in complexity), but it could easily grow and become
-- too slow.
--
module GameCollisions where

import Data.Hashable
import qualified Data.HashMap.Strict                   as HM
import Data.IdentityList
import Data.List
import Data.Maybe
import Debug.Trace
import Constants
import Objects
import Physics.TwoDimensions.Physics
import Physics.TwoDimensions.Dimensions

showPartitionMap :: (Hashable a, Show a) => HM.HashMap a [(ILKey, Object)] -> String
showPartitionMap hm = header ++ list
 where header = "*******************************"
       list   = foldr (\(q, os) s -> s ++ "\n" ++ show q ++ ":    " ++ objectNames os) "" (HM.toList hm)

       objectNames :: [(a, Object)] -> String
       objectNames os = concat $ intersperse "," oss
          where oss :: [String]
                oss = map (show . objectName . snd) os

-- | Given a list of objects, it detects all the collisions between them.
--
-- Note: this is a simple n*m-complex algorithm, with n the
-- number of objects and m the number of moving objects (right now,
-- only 2).
--
detectCollisions :: IL Object -> Collisions
detectCollisions = detectCollisionsH
 where detectCollisionsH objsT = -- trace (showPartitionMap segmented)
                                 flattened
         where -- Eliminate empty collision sets
               -- TODO: why is this really necessary?
               flattened = filter (\(Collision n) -> not (null n)) collisions

               -- Detect collisions between moving objects and any other objects
               -- collisions = detectCollisions' objsT moving

               segmented = partitionObjects objsT
               collisions = concatMap (\(o,(v,os)) -> concatMap (detectCollisions''' (o,v)) os) $ HM.toList (groupHashMap segmented)

               -- Partition the object space between moving and static objects
               (moving, _static) = partition (canCauseCollisions.snd) $ assocsIL objsT

groupHashMap :: Hashable a => HM.HashMap a [(ILKey, b)] -> HM.HashMap ILKey (b, [(ILKey, b)])
groupHashMap m = foldr f HM.empty (HM.elems m)

   where groupPairs :: [(ILKey, b)] -> [(ILKey, (b, [(ILKey, b)]))]
         groupPairs bs = [(k, (v, deleteBy (\(k1,_) (k2,_) -> k1 == k2) (k,v) bs)) | b@(k,v) <- bs]

         f :: [(ILKey, b)] -> HM.HashMap ILKey (b, [(ILKey, b)]) -> HM.HashMap ILKey (b, [(ILKey, b)])
         f bs hm = foldr (\(k, vs) hm' -> HM.insertWith (\(_,v1) (v,v2) -> (v, v1 ++ v2)) k vs hm') hm (groupPairs bs)

type Quadrant = (Int, Int)

partitionObjects :: IL Object -> HM.HashMap Quadrant [(ILKey,Object)]
partitionObjects objs = hm
    where hm = foldr (\(n,o) hmr -> insertObjectIntoQuadrants n o hmr) HM.empty (assocsIL objs)

          insertObjectIntoQuadrants on o hmr = foldr (insertObjectIntoQuadrant (on, o)) hmr (quadrants o)

          insertObjectIntoQuadrant on q hmr =
            HM.insertWith (\[on] os2 -> on : os2) q [on] hmr

          quadrants :: Object -> [Quadrant]
          quadrants o = [(x,y) | x <- xQuads, y <- yQuads]
            where (Rectangle (x,y) (w,h)) = objShape o
                  xQuads = [floor (minX / quadSide) .. ceiling (maxX / quadSide)]
                  yQuads = [floor (minY / quadSide) .. ceiling (maxY / quadSide)]
                  quadSide = 60
                  minX = x
                  maxX = x + w
                  minY = y
                  maxY = y + h

-- | Detect collisions between each moving object and
-- every other object.
detectCollisions' :: IL Object -> [(ILKey, Object)] -> [Collision]
detectCollisions' objsT ms = concatMap (detectCollisions'' objsT) ms

-- | Detect collisions between one specific moving object and every existing
-- object. Each collision is idependent of the rest (which is not necessarily
-- what should happen, but since the transformed velocities are eventually
-- added, there isn't much difference in the end).
detectCollisions'' :: IL Object -> (ILKey, Object) -> [Collision]
detectCollisions'' objsT m = concatMap (detectCollisions''' m) (assocsIL objsT)

-- | Detect a possible collision between two objects. Uses the object's keys to
-- distinguish them. Uses the basic 'Object'-based 'detectCollision' to
-- determine whether the two objects do collide.
detectCollisions''' :: (ILKey, Object) -> (ILKey, Object) -> [Collision]
detectCollisions''' m o
 | fst m == fst o = []    -- Same object -> no collision
 | otherwise      = maybeToList (detectCollision (snd m) (snd o))

-- updateObjPos :: SF (ILKey, Object) (ILKey, Object)
-- updateObjPos = proc (i,o) -> do
--   -- Since we are saving the position to avoid having to keep the last known
--   -- position in memory every time and integrate over a range every time
--   -- (would that really happen???) we use an integral over an interval.
--   -- I really wonder if this integration thing in Yampa works the way it is
--   -- expected to work. Does it work well for non-linear equations?
--   --
--   -- Integral only for dt interval
--   actualVel <- iterFrom (\_ (v1,v2) dt _ -> (v1 * dt, v2 * dt)) (0,0) -< objectVel o
--
--   -- Update position
--   let newPos = objectPos o ^+^ actualVel
--       o'     = o { objectPos = newPos }
--   returnA -< (i,o')

-- killBall :: ObjectOutput -> ObjectOutput
-- killBall oo = oo { outputObject = o' }
--  where o  = outputObject oo
--        o' = o { objectDead = True}

-- | Return the new velocity as changed by the collection of collisions.
--
-- HN 2014-09-07: New interface to collision detection.
--
-- The assumption is that collision detection happens globally and that the
-- changed velocity is figured out for each object involved in a collision
-- based on the properties of all objects involved in any specific interaction.
-- That may not be how it works now, but the interface means it could work
-- that way. Even more physical might be to figure out the impulsive force
-- acting on each object.
--
-- However, the whole collision infrastructure should be revisited.
--
-- - Statefulness ("edge") might make it more robust.
--
-- - Think through how collision events are going to be communicated
--   to the objects themselves. Maybe an input event is the natural
--   thing to do. Except then we have to be careful to avoid switching
--   again immediately after one switch.
--
-- - Should try to avoid n^2 checks. Maybe some kind of quad-trees?
--   Maybe spawning a stateful collision detector when two objects are
--   getting close? Cf. the old tail-gating approach.
-- - Maybe a collision should also carry the identity of the object
--   one collieded with to facilitate impl. of "inCollisionWith".
--
changedVelocity :: ObjectName -> Collisions -> Maybe Vel2D
changedVelocity name cs =
    case concatMap (filter ((== name) . fst) . collisionData) cs of
        []          -> Nothing
        (_, v') : _ -> Just v'

		-- IP: It should be something like the following, but that doesn't
		-- work:
        -- vs -> Just (foldl (^+^) (0,0) (map snd vs))

-- | True if the velocity of the object has been changed by any collision.
inCollision :: ObjectName -> Collisions -> Bool
inCollision name cs = isJust (changedVelocity name cs)

-- | True if the two objects are colliding with one another.
inCollisionWith :: ObjectName -> ObjectName -> Collisions -> Bool
inCollisionWith nm1 nm2 cs = any (both nm1 nm2) cs
    where
        both nm1 nm2 (Collision nmvs) =
            any ((== nm1) . fst) nmvs
            && any ((== nm2) . fst) nmvs
