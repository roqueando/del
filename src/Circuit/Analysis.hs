{-# LANGUAGE ScopedTypeVariables #-}

-- | Circuit analysis engine (Modified Nodal Analysis), ported from Paul
-- Falstad's Java circuit simulator.
--
-- Builds the circuit matrix from the placed elements, stamps the linear
-- elements (wire, resistor, voltage source, and the trapezoidal companion
-- models of capacitors and inductors), and solves the system with Gaussian
-- elimination (partial pivoting). Non-linear elements (the diode) are solved
-- with Newton-Raphson iteration.
--
-- The matrix-simplification pass (RowInfo) is not implemented yet.
module Circuit.Analysis
  ( Model (..),
    DynState (..),
    initialDynState,
    timeStep,
    resolveCircuit,
    transientStep,
    simulate,
  )
where

import Control.Monad (foldM, forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array (Array, accumArray, elems)
import Data.Array.MArray (freeze, newArray, readArray, thaw, writeArray)
import Data.Array.ST (STArray)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Circuit.Elements

-- | Default simulation time step (seconds), as in the original.
timeStep :: Double
timeStep = 5e-6

-- | An element with its node numbers resolved and its parameters.
data Model
  = MWire {mN0 :: !Int, mN1 :: !Int}
  | MResistor {mN0 :: !Int, mN1 :: !Int, mRes :: !Double}
  | MBattery {mN0 :: !Int, mN1 :: !Int, mVolt :: !Double, mVs :: !Int}
  | MCapacitor {mN0 :: !Int, mN1 :: !Int, mCap :: !Double}
  | MInductor {mN0 :: !Int, mN1 :: !Int, mInd :: !Double}
  | MDiode {mN0 :: !Int, mN1 :: !Int}
  deriving (Show)

-- | Per-element dynamic state, threaded between time steps. Node 0 is the
-- ground node. @dsAux@ carries the diode's last voltage difference.
data DynState = DynState
  { dsV0 :: !Double, -- ^ voltage at the first post (previous step)
    dsV1 :: !Double, -- ^ voltage at the second post (previous step)
    dsCurrent :: !Double, -- ^ current through the element (previous step)
    dsAux :: !Double -- ^ diode: last voltage difference (Newton state)
  }
  deriving (Show)

-- | Zero initial conditions.
initialDynState :: DynState
initialDynState = DynState 0 0 0 0

-- Diode model constants (from Diode.setup with the default forward drop).
leakage :: Double
leakage = 1e-14

fwdrop :: Double
fwdrop = 0.805904783

vdcoef :: Double
vdcoef = log (1 / leakage + 1) / fwdrop

vt :: Double
vt = 1 / vdcoef

vcrit :: Double
vcrit = vt * log (vt / (sqrt 2 * leakage))

-- | Resolves node numbers from coincident posts.
--
-- Returns @(models, nodeCount, vsCount)@. Node 0 is ground: the first terminal
-- of the first battery if one is present, otherwise a virtual ground node.
resolveCircuit :: [Element] -> ([Model], Int, Int)
resolveCircuit els = (models, nodeCount, vsCount)
  where
    ground = case [p1 | Element Battery _ p1 _ <- els] of
      p : _ -> p
      [] -> (-1, -1)

    allPoints = nub (ground : [p | Element _ _ p1 p2 <- els, p <- [p1, p2]])
    nodeIndex = Map.fromList (zip allPoints [0 ..])
    nodeCount = length allPoints
    nodeOf p = nodeIndex Map.! p

    (models, vsCount) = go 0 els
    go _ [] = ([], 0)
    go vs (Element kind val p1 p2 : rest) =
      let n0 = nodeOf p1
          n1 = nodeOf p2
          (thisModel, vsInc) = case kind of
            Battery -> (MBattery n0 n1 val vs, 1)
            _ -> (plain kind val n0 n1, 0)
          (restModels, restVs) = go (vs + vsInc) rest
       in (thisModel : restModels, vsInc + restVs)

    plain Wire _ n0 n1 = MWire n0 n1
    plain Resistor val n0 n1 = MResistor n0 n1 val
    plain Capacitor val n0 n1 = MCapacitor n0 n1 val
    plain Inductor val n0 n1 = MInductor n0 n1 val
    plain Diode _ n0 n1 = MDiode n0 n1
    plain Battery _ n0 n1 = MWire n0 n1 -- unreachable

-- | Advances the simulation by one time step.
--
-- Returns @(nodeVoltages, sourceCurrents, newState)@. @nodeVoltages@ has one
-- entry per node (index 0 is ground, always 0); @sourceCurrents@ has one entry
-- per voltage source.
transientStep ::
  [Model] ->
  [DynState] ->
  Int ->
  Int ->
  Double ->
  Either String ([Double], [Double], [DynState])
transientStep models dyn0 nodeCount vsCount dt
  | n <= 0 = Right (replicate nodeCount 0, replicate vsCount 0, dyn0)
  | otherwise =
      let (linM, linR) = linearStamps models nodeCount dt
       in if any isDiode models
            then newton linM linR (0 :: Int) dyn0
            else oneShot linM linR dyn0
  where
    n = nodeCount - 1 + vsCount

    oneShot linM linR dyn = do
      let (dynM, dynR, dyn', _) = dynamicStamps models dyn dt
      sol <- buildAndSolve linM linR dynM dynR
      let (v, src) = extract nodeCount (elems sol)
      return (v, src, updateAfterSolve models dyn' src v dt)

    newton linM linR subiter dyn
      | subiter > 5000 = Left "convergence failed"
      | otherwise = do
          let (dynM, dynR, dyn', converged) = dynamicStamps models dyn dt
          sol <- buildAndSolve linM linR dynM dynR
          let (v, src) = extract nodeCount (elems sol)
              dyn'' = updateAfterSolve models dyn' src v dt
          if converged && subiter > 0
            then return (v, src, dyn'')
            else newton linM linR (subiter + 1) dyn''

    buildAndSolve linM linR dynM dynR = do
      let matrix = accumArray (+) 0 ((0, 0), (n - 1, n - 1)) [((i, j), v) | (i, j, v) <- linM ++ dynM]
          rhs = accumArray (+) 0 (0, n - 1) (linR ++ dynR)
      gaussSolve n matrix rhs

    extract nc sol = (0 : take (nc - 1) sol, drop (nc - 1) sol)

-- | Runs @n@ transient steps, returning the final node voltages and state.
simulate ::
  [Model] ->
  [DynState] ->
  Int ->
  Int ->
  Double ->
  Int ->
  Either String ([Double], [DynState])
simulate models dyn0 nodeCount vsCount dt n = go n dyn0 (replicate nodeCount 0)
  where
    go 0 d v = Right (v, d)
    go k d _ = case transientStep models d nodeCount vsCount dt of
      Left e -> Left e
      Right (v, _, d') -> go (k - 1) d' v

-- Stamping --------------------------------------------------------------

type MStamp = (Int, Int, Double)
type RStamp = (Int, Double)

isDiode :: Model -> Bool
isDiode MDiode {} = True
isDiode _ = False

-- | Linear stamps, computed once per time step (and reused every Newton
-- iteration for non-linear circuits).
linearStamps :: [Model] -> Int -> Double -> ([MStamp], [RStamp])
linearStamps models nodeCount dt = foldMap step models
  where
    step model = case model of
      MWire n0 n1 -> stampResistor n0 n1 1e-6
      MResistor n0 n1 r -> stampResistor n0 n1 r
      MBattery n0 n1 v vs -> stampVoltageSource nodeCount n0 n1 vs v
      MCapacitor n0 n1 c -> stampResistor n0 n1 (dt / (2 * c))
      MInductor n0 n1 l -> stampResistor n0 n1 (2 * l / dt)
      MDiode _ _ -> ([], [])

-- | Dynamic stamps for one Newton iteration: the history current sources of
-- capacitors/inductors and the linearized diode. Returns the updated state
-- (diode @dsAux@ advances) and whether this iteration converged.
dynamicStamps :: [Model] -> [DynState] -> Double -> ([MStamp], [RStamp], [DynState], Bool)
dynamicStamps models dyn dt = go ([], [], [], True) (zip models dyn)
  where
    go (ms, rs, ds, conv) [] = (ms, rs, reverse ds, conv)
    go (ms, rs, ds, conv) ((model, st) : rest) = case model of
      MCapacitor n0 n1 c ->
        let compR = dt / (2 * c)
            voltdiff = dsV0 st - dsV1 st
            curSource = -voltdiff / compR - dsCurrent st
            (m, r) = stampCurrentSource n0 n1 curSource
         in go (ms ++ m, rs ++ r, st : ds, conv) rest
      MInductor n0 n1 l ->
        let compR = 2 * l / dt
            voltdiff = dsV0 st - dsV1 st
            curSource = voltdiff / compR + dsCurrent st
            (m, r) = stampCurrentSource n0 n1 curSource
         in go (ms ++ m, rs ++ r, st : ds, conv) rest
      MDiode n0 n1 ->
        let voltdiff = dsV0 st - dsV1 st
            lastVd = dsAux st
            changedTooMuch = abs (voltdiff - lastVd) > 0.01
            vd = limitStep voltdiff lastVd
            clamped = vd /= voltdiff
            eval = exp (vd * vdcoef)
            eval' = if vd < 0 then 1 else eval
            geq = vdcoef * leakage * eval'
            nc = (eval' - 1) * leakage - geq * vd
            (m1, r1) = stampConductance n0 n1 geq
            (m2, r2) = stampCurrentSource n0 n1 nc
            st' = st {dsAux = vd}
         in go (ms ++ m1 ++ m2, rs ++ r1 ++ r2, st' : ds, conv && not changedTooMuch && not clamped) rest
      _ -> go (ms, rs, st : ds, conv) rest

-- | Newton step limiter from the original (forward diode only).
limitStep :: Double -> Double -> Double
limitStep vnew vold
  | vnew > vcrit && abs (vnew - vold) > (vt + vt) =
      if vold > 0
        then
          let arg = 1 + (vnew - vold) / vt
           in if arg > 0 then max v0 (vold + vt * log arg) else vcrit
        else vt * log (vnew / vt)
  | otherwise = vnew
  where
    v0 = log (1e-6 / leakage) * vt

stampResistor :: Int -> Int -> Double -> ([MStamp], [RStamp])
stampResistor n0 n1 r =
  let g = 1 / r
      m =
        [(n0 - 1, n0 - 1, g) | n0 > 0]
          ++ [(n1 - 1, n1 - 1, g) | n1 > 0]
          ++ [(n0 - 1, n1 - 1, -g) | n0 > 0 && n1 > 0]
          ++ [(n1 - 1, n0 - 1, -g) | n0 > 0 && n1 > 0]
   in (m, [])

stampConductance :: Int -> Int -> Double -> ([MStamp], [RStamp])
stampConductance n0 n1 g =
  ( [ (n0 - 1, n0 - 1, g) | n0 > 0]
      ++ [(n1 - 1, n1 - 1, g) | n1 > 0]
      ++ [(n0 - 1, n1 - 1, -g) | n0 > 0 && n1 > 0]
      ++ [(n1 - 1, n0 - 1, -g) | n0 > 0 && n1 > 0],
    []
  )

stampVoltageSource :: Int -> Int -> Int -> Int -> Double -> ([MStamp], [RStamp])
stampVoltageSource nodeCount n0 n1 vs v =
  let vn = nodeCount - 1 + vs
      m =
        [(vn, n0 - 1, -1) | n0 > 0]
          ++ [(vn, n1 - 1, 1) | n1 > 0]
          ++ [(n0 - 1, vn, 1) | n0 > 0]
          ++ [(n1 - 1, vn, -1) | n1 > 0]
   in (m, [(vn, v)])

stampCurrentSource :: Int -> Int -> Double -> ([MStamp], [RStamp])
stampCurrentSource n0 n1 i =
  ([], [(n0 - 1, -i) | n0 > 0] ++ [(n1 - 1, i) | n1 > 0])

-- State update ----------------------------------------------------------

updateAfterSolve :: [Model] -> [DynState] -> [Double] -> [Double] -> Double -> [DynState]
updateAfterSolve models dyn srcCurrents nodeVolts dt =
  [ upd model st
    | (model, st) <- zip models dyn
  ]
  where
    v n = nodeVolts !! n

    upd model st = case model of
      MWire n0 n1 ->
        let vd = v n0 - v n1
         in st {dsV0 = v n0, dsV1 = v n1, dsCurrent = vd / 1e-6}
      MResistor n0 n1 r ->
        let vd = v n0 - v n1
         in st {dsV0 = v n0, dsV1 = v n1, dsCurrent = vd / r}
      MBattery n0 n1 _ vs ->
        st {dsV0 = v n0, dsV1 = v n1, dsCurrent = srcCurrents !! vs}
      MCapacitor n0 n1 c ->
        let compR = dt / (2 * c)
            voltdiff = v n0 - v n1
            curSource = -(dsV0 st - dsV1 st) / compR - dsCurrent st
            current = voltdiff / compR + curSource
         in st {dsV0 = v n0, dsV1 = v n1, dsCurrent = current}
      MInductor n0 n1 l ->
        let compR = 2 * l / dt
            voltdiff = v n0 - v n1
            curSource = (dsV0 st - dsV1 st) / compR + dsCurrent st
            current = voltdiff / compR + curSource
         in st {dsV0 = v n0, dsV1 = v n1, dsCurrent = current}
      MDiode n0 n1 ->
        let voltdiff = v n0 - v n1
            current = leakage * (exp (voltdiff * vdcoef) - 1)
         in st {dsV0 = v n0, dsV1 = v n1, dsCurrent = current}

-- Gaussian elimination with partial pivoting ---------------------------

gaussSolve :: Int -> Array (Int, Int) Double -> Array Int Double -> Either String (Array Int Double)
gaussSolve n a b = runST $ do
  m <- thaw a :: ST s (STArray s (Int, Int) Double)
  x <- thaw b :: ST s (STArray s Int Double)
  r <- eliminate n m x 0
  case r of
    Left msg -> return (Left msg)
    Right () -> do
      sol <- backSubst n m x
      frozen <- freeze sol
      return (Right frozen)

eliminate :: Int -> STArray s (Int, Int) Double -> STArray s Int Double -> Int -> ST s (Either String ())
eliminate n m x k
  | k == n = return (Right ())
  | otherwise = do
      piv <- findPivot n m k
      if piv < 0
        then return (Left "singular matrix")
        else do
          when (piv /= k) $ swapRows n m x k piv
          pivotVal <- readArray m (k, k)
          forM_ [k + 1 .. n - 1] $ \i -> do
            aik <- readArray m (i, k)
            when (aik /= 0) $ do
              let factor = aik / pivotVal
              forM_ [k .. n - 1] $ \j -> do
                aij <- readArray m (i, j)
                akj <- readArray m (k, j)
                writeArray m (i, j) (aij - factor * akj)
              bi <- readArray x i
              bk <- readArray x k
              writeArray x i (bi - factor * bk)
          eliminate n m x (k + 1)

findPivot :: forall s. Int -> STArray s (Int, Int) Double -> Int -> ST s Int
findPivot n m k = go k (-1) (-1)
  where
    go :: Int -> Int -> Double -> ST s Int
    go i best bestVal
      | i == n = return best
      | otherwise = do
          v <- readArray m (i, k)
          let av = abs v
          if av > bestVal then go (i + 1) i av else go (i + 1) best bestVal

swapRows :: Int -> STArray s (Int, Int) Double -> STArray s Int Double -> Int -> Int -> ST s ()
swapRows n m x r1 r2 = do
  forM_ [0 .. n - 1] $ \j -> do
    t <- readArray m (r1, j)
    u <- readArray m (r2, j)
    writeArray m (r1, j) u
    writeArray m (r2, j) t
  t <- readArray x r1
  u <- readArray x r2
  writeArray x r1 u
  writeArray x r2 t

backSubst :: Int -> STArray s (Int, Int) Double -> STArray s Int Double -> ST s (STArray s Int Double)
backSubst n m x = do
  sol <- newArray (0, n - 1) 0
  forM_ [n - 1, n - 2 .. 0] $ \i -> do
    bi <- readArray x i
    s <- foldM (\acc j -> do
                  aij <- readArray m (i, j)
                  xj <- readArray sol j
                  return (acc - aij * xj)) bi [i + 1 .. n - 1]
    aii <- readArray m (i, i)
    writeArray sol i (s / aii)
  return sol
