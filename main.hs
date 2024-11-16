module Main where

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
type Wave = Signal Pulse

instance Num a => Num (Signal a) where
    Sig f + Sig g = Sig (\t -> f t + g t)
    Sig f - Sig g = Sig (\t -> f t - g t)
    Sig f * Sig g = Sig (\t -> f t * g t)
    abs (Sig f) = Sig (abs . f)
    signum (Sig f) = Sig (signum . f)
    fromInteger = Sig . const . fromInteger

instance Functor Signal where
    fmap f (Sig g) = Sig (fmap f g)

signalToBytes :: Wave -> BL.ByteString
signalToBytes (Sig f) = toLazyByteString $ foldMap (floatLE . f) [0.0, 1 / sampleRate ..]

save :: FilePath -> Seconds -> Wave -> IO ()
save path time = BL.writeFile path . BL.take (round (sampleRate * time)) . signalToBytes

play :: Wave -> IO ()
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

sine :: Hz -> Wave
sine hz = Sig (\t -> sin (2 * pi * t * hz))

square :: Hz -> Wave
square hz = Sig (\t -> 4 * down (t * hz) - 2 * down (2 * t * hz) + 1)

triangle :: Hz -> Wave
triangle hz = Sig (\t -> 4 * abs (t * hz - down (t * hz + 3 / 4) + 1 / 4) - 1)

saw :: Hz -> Wave
saw hz = Sig (\t -> 2 * (t * hz - down (t * hz + 1 / 2)))

major :: Wave
major = fmap (* 0.3) $ sum $ map (sine . semitoneToHz) [0, 4, 7]
  where
    semitoneToHz n = 440.0 * (2 ** (1 / 12)) ** n
    -- attack rate = zipWith (*) (map (min 1.0) [0.0, rate ..])

beat :: Wave
beat = (* 0.5) <$> sine 440 + sine 442

overtones :: Wave
overtones =
    fmap (* 0.5)
    $ sum
    $ zipWith (fmap . (*)) volumes
    $ map (sine . (110 *)) [1 .. count]
  where
    count = 6
    volumes = [1 - i / count | i <- [0 ..]]

main :: IO ()
main = play overtones

-- main :: IO ()
-- main = play $ clamped $ muted $ intercalate silence waves
--   where
--     waves = [sine 1 440, square 1 440, triangle 1 440, saw 1 440, major 2, beat 2, overtones]
--     silence = sine 0.2 0
--     muted = map (* 0.3)
--     clamped = map (min 1.0 . max (-1.0))
