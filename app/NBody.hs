{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

module NBody where

import Shoot
import Data.List (tails)

-- N-Body

days_per_year = 365.24 :: Double'
solar_mass = 4 * pi^2 :: Double'

data Body = Body
  { px :: Double'
  , py :: Double'
  , pz :: Double'
  , vx :: Double'
  , vy :: Double'
  , vz :: Double'
  , mass :: Double'
  }

instance Agg Body where
  aggRec = AggRec
    { _agg = \(Node xs) -> let [a,b,c,d,e,f,g] = map agg xs in Body a b c d e f g
    , _unAgg = \(Body a b c d e f g) -> Node $ map unAgg [a,b,c,d,e,f,g]
    , _typeofAgg = \x -> Node $ map typeofAgg [px x, py x, pz x, vx x, vy x, vz x, mass x]
    }

px_ f x = x{ px = f $ px x }
py_ f x = x{ py = f $ py x }
pz_ f x = x{ pz = f $ pz x }
vx_ f x = x{ vx = f $ vx x }
vy_ f x = x{ vy = f $ vy x }
vz_ f x = x{ vz = f $ vz x }
mass_ f x = x{ mass = f $ mass x }

bodies :: [Body]
bodies =
 map (vx_ (*days_per_year) . vy_ (*days_per_year) . vz_ (*days_per_year) . mass_ (*solar_mass))
  [ Body 0 0 0 0 0 0 1                 -- sun
  , Body                               -- jupiter
      4.84143144246472090e+00
      (-1.16032004402742839e+00)
      (-1.03622044471123109e-01)
      1.66007664274403694e-03
      7.69901118419740425e-03
      (-6.90460016972063023e-05)
      9.54791938424326609e-04
  , Body                               -- saturn
      8.34336671824457987e+00
      4.12479856412430479e+00
      (-4.03523417114321381e-01)
      (-2.76742510726862411e-03)
      4.99852801234917238e-03
      2.30417297573763929e-05
      2.85885980666130812e-04
  , Body                               -- uranus
      1.28943695621391310e+01
      (-1.51111514016986312e+01)
      (-2.23307578892655734e-01)
      2.96460137564761618e-03
      2.37847173959480950e-03
      (-2.96589568540237556e-05)
      4.36624404335156298e-05
  , Body                               -- neptune
      1.53796971148509165e+01
      (-2.59193146099879641e+01)
      1.79258772950371181e-01
      2.68067772490389322e-03
      1.62824170038242295e-03
      (-9.51592254519715870e-05)
      5.15138902046611451e-05
  ]

advance :: Double' -> [Body] -> [Body]
advance dt bs0 = map h $ adv' [] bs0
  where
    h b = px_ (f vx) $ py_ (f vy) $ pz_ (f vz) b
      where
        f v = (+) (dt * v b)
    adv' xs [] = reverse xs
    adv' xs (b:bs) = let (b':bs') = adv [] b bs in adv' (b':xs) bs'
    adv xs b [] = b : reverse xs
    adv xs b (b2:bs) = adv (b2' : xs) b' bs
      where
      f d v = v - d * mass b2 * mag
      g d v = v + d * mass b * mag
      b' = vx_ (f dx) $ vy_ (f dy) $ vz_ (f dz) b
      b2' = vx_ (g dx) $ vy_ (g dy) $ vz_ (g dz) b2
      dx = px b - px b2
      dy = py b - py b2
      dz = pz b - pz b2
      distance = sqrt(dx^2 + dy^2 + dz^2)
      mag = dt / (distance^3)

energy :: [Body] -> Double'
energy = sum . map enrgy . init . tails
  where
    enrgy :: [Body] -> Double'
    enrgy (b:bs) = 0.5 * mass b * ((vx b)^2 + (vy b)^2 + (vz b)^2) - sum (map f bs)
      where
        f b2 = (mass b * mass b2) / (sqrt ((g px)^2 + (g py)^2 + (g pz)^2))
          where g h = h b - h b2

offset_momentum :: [Body] -> [Body]
offset_momentum xs@(x : _) = (vx_ (f vx) $ vy_ (f vy) $ vz_ (f vz) x) : tail xs
  where
    f g = \_ -> -((sum $ map (\bdy -> g bdy * mass bdy) xs) / solar_mass)

nbody :: Int' -> Double'
nbody = func "nbody" $ \n -> energy $ reps n (offset_momentum bodies) (advance 0.01)
