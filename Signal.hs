module Signal where

import Data.ByteString.Builder (floatLE, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.List
import System.IO
import System.Process

type Seconds = Float
type Pulse = Float
type Hz = Float

sampleRate :: Hz
sampleRate = 48000.0

newtype Signal a = Sig (Seconds -> a)

instance Num a => Num (Signal a) where
    Sig f + Sig g = Sig (\t -> f t + g t)
    Sig f - Sig g = Sig (\t -> f t - g t)
    Sig f * Sig g = Sig (\t -> f t * g t)
    abs (Sig f) = Sig (abs . f)
    signum (Sig f) = Sig (signum . f)
    fromInteger = Sig . const . fromInteger

instance Functor Signal where
    fmap f (Sig g) = Sig (fmap f g)

instance Applicative Signal where
    pure = Sig . const
    Sig f <*> Sig g = Sig (\t -> f t (g t))

signalToBytes :: Signal Pulse -> BL.ByteString
signalToBytes (Sig f) = toLazyByteString $ foldMap (floatLE . f) [fromIntegral n / sampleRate | n <- [0..]]

save :: FilePath -> Seconds -> Signal Pulse -> IO ()
save path time = BL.writeFile path . BL.take (round (sampleRate * time)) . signalToBytes

play :: Signal Pulse -> IO ()
play signal = do
    let args =
            [ "-hide_banner"
            , "-loglevel" , "error"
            , "-autoexit"
            , "-showmode" , "1"
            , "-f"        , "f32le"
            , "-ar"       , show sampleRate
            , "pipe:0"
            ]
    (Just stdin, _, _, p) <- createProcess (proc "ffplay" args) {std_in = CreatePipe}
    BL.hPut stdin $ signalToBytes signal
    hFlush stdin
    hClose stdin
    waitForProcess p
    return ()

down :: Float -> Float
down x = fromIntegral (floor x :: Integer)

sine :: Hz -> Signal Pulse
sine hz = Sig (\t -> sin (2 * pi * t * hz))

square :: Hz -> Signal Pulse
square hz = Sig (\t -> 4 * down (t * hz) - 2 * down (2 * t * hz) + 1)

triangle :: Hz -> Signal Pulse
triangle hz = Sig (\t -> 4 * abs (t * hz - down (t * hz + 3 / 4) + 1 / 4) - 1)

saw :: Hz -> Signal Pulse
saw hz = Sig (\t -> 2 * (t * hz - down (t * hz + 1 / 2)))

seq :: [(Signal Pulse, Seconds)] -> Signal Pulse
seq signals = Sig $ \t -> go signals t t
  where
    go ((Sig f, duration) : xs) time
        | time < duration = f
        | otherwise       = go xs (time - duration)
    go [] _ = const 0

attack :: Seconds -> Signal (Pulse -> Pulse)
attack time = Sig (\t sig -> sig * min (t / time) 1.0)

clamp :: Signal Pulse -> Signal Pulse
clamp = fmap (min 1.0 . max (-1.0))
