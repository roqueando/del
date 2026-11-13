module Main (main) where

import Circuit.Analysis
import Circuit.Elements (ComponentKind (..), Element (..))

-- The example series RLC circuit (same as Circuit.rlcCircuit).
rlc :: [Element]
rlc =
  [ Element Battery 5 (240, 144) (240, 416),
    Element Resistor 100 (240, 144) (560, 144),
    Element Inductor 1 (560, 144) (560, 416),
    Element Capacitor 1e-5 (560, 416) (240, 416)
  ]

-- A forward-biased diode in series with a resistor and battery.
diodeCircuit :: [Element]
diodeCircuit =
  [ Element Battery 5 (240, 144) (240, 416),
    Element Resistor 100 (240, 144) (560, 144),
    Element Diode 0.805904783 (560, 416) (560, 144), -- anode at bottom-right
    Element Wire 1e-6 (560, 416) (240, 416)
  ]

main :: IO ()
main = do
  rlcChecks
  diodeChecks
  putStrLn "all checks passed"

rlcChecks :: IO ()
rlcChecks = do
  let (models, nodeCount, vsCount) = resolveCircuit rlc

  check "node count is 4" (nodeCount == 4)
  check "voltage source count is 1" (vsCount == 1)

  let dyn0 = replicate (length models) initialDynState
      dt = timeStep

  -- One step: the battery forces its positive terminal (ground) to 0 V and
  -- its negative terminal to -5 V.
  case transientStep models dyn0 nodeCount vsCount dt of
    Left e -> fail ("first step failed: " ++ e)
    Right (v, src, d1) -> do
      check "ground node is 0 V" (abs (v !! 0) < 1e-12)
      check "battery positive terminal is +5 V" (abs (v !! 1 - 5) < 1e-9)
      check "source currents are finite" (all finite src)

      -- Run the transient and check the capacitor charges toward -5 V
      -- (its plate at the battery positive node is at +5 V).
      (vFinal, _) <- runSteps models d1 nodeCount vsCount dt 20000
      let capVoltage = vFinal !! 3 - vFinal !! 1
      putStrLn ("capacitor voltage after 20000 steps: " ++ show capVoltage)
      check "capacitor voltage is finite" (finite capVoltage)
      check "capacitor voltage is negative" (capVoltage < 0)
      check "capacitor voltage approaches -5 V" (abs (capVoltage + 5) < 1.0)

diodeChecks :: IO ()
diodeChecks = do
  let (models, nodeCount, vsCount) = resolveCircuit diodeCircuit
  check "diode node count is 4" (nodeCount == 4)
  let dyn0 = replicate (length models) initialDynState
  case transientStep models dyn0 nodeCount vsCount timeStep of
    Left e -> fail ("diode step failed: " ++ e)
    Right (v, _, _) -> do
      let diodeVoltage = v !! 3 - v !! 2
          resistorCurrent = (v !! 2 - v !! 0) / 100
      putStrLn ("diode voltage: " ++ show diodeVoltage)
      putStrLn ("resistor current: " ++ show resistorCurrent)
      check "diode is forward biased" (diodeVoltage > 0.3 && diodeVoltage < 1.0)
      check "current is positive" (resistorCurrent > 0.001)
      check "current is finite" (finite resistorCurrent)

finite :: Double -> Bool
finite x = not (isNaN x) && not (isInfinite x)

runSteps :: [Model] -> [DynState] -> Int -> Int -> Double -> Int -> IO ([Double], [DynState])
runSteps models dyn0 nodeCount vsCount dt n = go n dyn0 (replicate nodeCount 0)
  where
    go 0 d v = pure (v, d)
    go k d _ = do
      case transientStep models d nodeCount vsCount dt of
        Left e -> fail ("step failed: " ++ e)
        Right (v, _, d') -> go (k - 1) d' v

check :: String -> Bool -> IO ()
check name ok = do
  putStrLn ((if ok then "PASS " else "FAIL ") ++ name)
  if ok then pure () else fail name
