module Main where

import Signal

major :: Signal Pulse
major = fmap (* 0.2) $ sum (chords sine) + sum (chords triangle)
  where
    semitoneToHz n = 440.0 * (2 ** (1 / 12)) ** n
    chords mkWave = map (mkWave . semitoneToHz) [0, 4, 7]

beat :: Signal Pulse
beat = (* 0.5) <$> sine 440 + sine 442

overtones :: Signal Pulse
overtones =
    fmap (* 0.5)
    $ sum
    $ zipWith (fmap . (*)) volumes
    $ map (sine . (165 *)) [1 .. count]
  where
    count = 6
    volumes = [1 - i / count | i <- [0 ..]]

main :: IO ()
main = play $ clamp $ pure 0.3 * Signal.seq loop
  where
    loop = (attack 2 <*> major, 3) : (beat, 3) : (overtones, 3) : loop
