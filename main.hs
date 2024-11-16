module Main where

import Data.ByteString.Builder (floatLE, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.List
import System.IO
import System.Process

type Wave = [Float]
type Seconds = Float
type Hz = Float

sampleRate :: Hz
sampleRate = 48000.0

save :: FilePath -> Wave -> IO ()
save path = BL.writeFile path . toLazyByteString . foldMap floatLE

play :: Wave -> IO ()
play wave = do
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
    BL.hPut stdin $ toLazyByteString (foldMap floatLE wave)
    hFlush stdin
    hClose stdin
    waitForProcess p
    return ()

timestamps :: Seconds -> [Seconds]
timestamps time = [0.0, 1 / sampleRate .. time]

mkWave :: (Seconds -> Float) -> Seconds -> Hz -> Wave
mkWave f time hz = map (f . (hz *)) $ timestamps time

down :: Float -> Float
down x = fromIntegral (floor x :: Integer)

sine :: Seconds -> Hz -> Wave
sine = mkWave (\t -> sin (2 * pi * t))

square :: Seconds -> Hz -> Wave
square = mkWave (\t -> 4 * down t - 2 * down (2 * t) + 1)

triangle :: Seconds -> Hz -> Wave
triangle = mkWave (\t -> 4 * abs (t - down (t + 3 / 4) + 1 / 4) - 1)

saw :: Seconds -> Hz -> Wave
saw = mkWave (\t -> 2 * (t - down (t + 1 / 2)))

major :: Seconds -> Wave
major time =
    attack 0.0001
    $ map sum . transpose
    $ map (sine time . semitoneToHz) [0, 4, 7]
  where
    semitoneToHz n = 440.0 * (2 ** (1 / 12)) ** n
    attack rate = zipWith (*) (map (min 1.0) [0.0, rate ..])

beat :: Seconds -> Wave
beat time = zipWith (+) (sine time 440) (sine time 442)

overtones :: Wave
overtones =
    map sum . transpose
    $ zipWith (map . (*)) volumes
    $ map (sine 3) freqs
  where
    count = 6
    volumes = [1 - i / count | i <- [0 ..]]
    freqs = map (110 *) [1 .. count]

main :: IO ()
main = play $ clamped $ muted $ intercalate silence waves
  where
    waves = [sine 1 440, square 1 440, triangle 1 440, saw 1 440, major 2, beat 2, overtones]
    silence = sine 0.2 0
    muted = map (* 0.3)
    clamped = map (min 1.0 . max (-1.0))
