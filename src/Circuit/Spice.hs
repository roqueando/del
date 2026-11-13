{-# LANGUAGE ForeignFunctionInterface #-}

-- | Binding to libngspice plus netlist generation.
--
-- The circuit is translated to a SPICE netlist; ngspice runs a transient in a
-- background thread and streams every accepted time point through its
-- @SendData@ callback. The C bridge caches the latest values, which this
-- module polls by vector name.
module Circuit.Spice
  ( SpiceElement (..),
    generateNetlist,
    initSpice,
    loadNetlist,
    spiceCommand,
    spiceGetValue,
    spiceSetDelay,
    spiceRunning,
    readVector,
    nodeVoltageName,
  )
where

import Circuit.Analysis (Model (..), resolveCircuit)
import Circuit.Elements (Element (..))
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CDouble (..), CInt (..))

-- | Mapping from a placed element back to its SPICE vectors.
data SpiceElement = SpiceElement
  { seNode0 :: Int, -- ^ node index of the first post
    seNode1 :: Int, -- ^ node index of the second post
    seCurrentName :: String -- ^ vector name for the element's current
  }
  deriving (Show)

foreign import ccall "bridge_ngspice_init" c_ngspiceInit :: IO ()
foreign import ccall "bridge_ngspice_command" c_ngspiceCommand :: CString -> IO ()
foreign import ccall "bridge_ngspice_load_netlist" c_ngspiceLoadNetlist :: CString -> IO ()
foreign import ccall "bridge_ngspice_get_value" c_ngspiceGetValue :: CString -> IO CDouble
foreign import ccall "bridge_ngspice_set_delay" c_ngspiceSetDelay :: CInt -> IO ()
foreign import ccall "bridge_ngspice_running" c_ngspiceRunning :: IO CInt
foreign import ccall "bridge_ngspice_get_vector_length" c_ngspiceGetVectorLength :: CString -> IO CInt
foreign import ccall "bridge_ngspice_get_vector_value" c_ngspiceGetVectorValue :: CString -> CInt -> IO CDouble

-- | Initializes ngspice (call once at startup).
initSpice :: IO ()
initSpice = c_ngspiceInit

-- | Sends an interactive command.
spiceCommand :: String -> IO ()
spiceCommand cmd = withCString cmd c_ngspiceCommand

-- | Loads a netlist given as one newline-separated string.
loadNetlist :: String -> IO ()
loadNetlist net = withCString net c_ngspiceLoadNetlist

-- | Reads the latest value of a named vector (0.0 if unknown).
spiceGetValue :: String -> IO Double
spiceGetValue name = withCString name $ \n -> realToFrac <$> c_ngspiceGetValue n

-- | Sets the per-data-point delay (microseconds) used to throttle animation.
spiceSetDelay :: Int -> IO ()
spiceSetDelay us = c_ngspiceSetDelay (fromIntegral us)

-- | Whether the background simulation thread is currently running.
spiceRunning :: IO Bool
spiceRunning = (/= 0) <$> c_ngspiceRunning

-- | Reads a full output vector into a list.
readVector :: String -> IO [Double]
readVector name = do
  len <- (fromIntegral <$> withCString name c_ngspiceGetVectorLength) :: IO Int
  mapM (\i -> withCString name $ \n -> realToFrac <$> c_ngspiceGetVectorValue n (fromIntegral i)) [0 .. len - 1]

-- | Vector name for a node's voltage.
nodeVoltageName :: Int -> String
nodeVoltageName i = "V(" ++ show i ++ ")"

-- | Netlist node name (0 is ground).
nodeName :: Int -> String
nodeName 0 = "0"
nodeName i = show i

-- | Generates a SPICE netlist from the placed elements, together with the
-- per-element mapping used to read back voltages and currents, and the node
-- count.
generateNetlist :: [Element] -> (String, [SpiceElement], Int)
generateNetlist els =
  let (models, nodeCount, _) = resolveCircuit els
      (revLines, revElems, revSaves) = go (0 :: Int) 0 0 0 0 [] [] [] models
      saves = reverse revSaves
      nodeVoltSaves = ["V(" ++ show i ++ ")" | i <- [1 .. nodeCount - 1]]
      saveLine = ".save " ++ unwords (nodeVoltSaves ++ saves)
      body = reverse revLines
      netlist = unlines (["circuit"] ++ body ++ [saveLine] ++ [".end"])
   in (netlist, reverse revElems, nodeCount)
  where
    go :: Int -> Int -> Int -> Int -> Int -> [String] -> [SpiceElement] -> [String] -> [Model] -> ([String], [SpiceElement], [String])
    go _ _ _ _ _ ls es ss [] = (ls, es, ss)
    go r c l v d ls es ss (m : ms) =
      case m of
        MWire n0 n1 ->
          let dev = "r" ++ show (r + 1)
              line = dev ++ " " ++ nodeName n0 ++ " " ++ nodeName n1 ++ " 1e-6"
              cur = "@" ++ dev ++ "[i]"
           in go (r + 1) c l v d (line : ls) (SpiceElement n0 n1 cur : es) (cur : ss) ms
        MResistor n0 n1 res ->
          let dev = "r" ++ show (r + 1)
              line = dev ++ " " ++ nodeName n0 ++ " " ++ nodeName n1 ++ " " ++ show res
              cur = "@" ++ dev ++ "[i]"
           in go (r + 1) c l v d (line : ls) (SpiceElement n0 n1 cur : es) (cur : ss) ms
        MBattery n0 n1 volt _ ->
          let dev = "v" ++ show (v + 1)
              line = dev ++ " " ++ nodeName n1 ++ " " ++ nodeName n0 ++ " DC " ++ show volt
              cur = dev ++ "#branch"
           in go r c l (v + 1) d (line : ls) (SpiceElement n0 n1 cur : es) (cur : ss) ms
        MCapacitor n0 n1 cap ->
          let dev = "c" ++ show (c + 1)
              line = dev ++ " " ++ nodeName n0 ++ " " ++ nodeName n1 ++ " " ++ show cap
              cur = "@" ++ dev ++ "[i]"
           in go r (c + 1) l v d (line : ls) (SpiceElement n0 n1 cur : es) (cur : ss) ms
        MInductor n0 n1 ind ->
          let dev = "l" ++ show (l + 1)
              line = dev ++ " " ++ nodeName n0 ++ " " ++ nodeName n1 ++ " " ++ show ind
              cur = dev ++ "#branch"
           in go r c (l + 1) v d (line : ls) (SpiceElement n0 n1 cur : es) (cur : ss) ms
        MDiode n0 n1 ->
          let dev = "d" ++ show (d + 1)
              line = dev ++ " " ++ nodeName n0 ++ " " ++ nodeName n1
              cur = "@" ++ dev ++ "[i]"
           in go r c l v (d + 1) (line : ls) (SpiceElement n0 n1 cur : es) (cur : ss) ms
