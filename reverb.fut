-- % Reverb in Futhark
-- % Philip Munksgaard
-- % March 10, 2023

-- Inspired by: https://shwestrick.github.io/2020/08/28/digital-reverb.html

def cap [n] (xs: [n]f64) =
  map (\x -> x / f64.maximum xs) xs

def seq_comb [n] (delay: i64) (intensity: f64) (xs: [n]f64): [n]f64 =
  loop xs = copy xs for i in delay..<n do
  let xs[i] = xs[i] + intensity * xs[i - delay]
  in xs

def par_comb [n] (delay: i64) (intensity: f64) (xs: [n]f64): [n]f64 =
  let n' = n + delay - (n % delay)
  let padded = replicate n' 0.0f64
  let padded[:n] = xs
  let xss = unflatten (n' / delay) delay padded |> transpose
  let res = map (scan (\x y -> y + intensity * x) 0) xss
            |> transpose
            |> flatten_to n'
  in res[:n]

entry comb delay intensity = map (cap <-< seq_comb delay intensity)

def allpass [n] (c : i64 -> f64 -> [n]f64 -> [n]f64) (delay: i64) (intensity: f64) (xs: [n]f64): [n]f64 =
  let combed = c delay intensity xs
  in loop xs = copy xs for i < n do
     let xs[i] = (-intensity) * xs[i] + (1 - intensity**2) * (if i < delay then 0 else combed[i-delay])
     in xs

def reverb' [n] (c : i64 -> f64 -> [n]f64 -> [n]f64) (xs: [n]f64) =
  let cs1 = c 1931 0.7 xs
  let cs2 = c 2213 0.7 xs
  let cs3 = c 1747 0.7 xs
  let cs4 = c 1559 0.7 xs
  in map4 (\c1 c2 c3 c4 -> c1 + c2 + c3 + c4) cs1 cs2 cs3 cs4
     |> allpass c 167 0.7
     |> allpass c 191 0.7
     |> cap

entry reverb_seq = map (reverb' seq_comb)

entry reverb_par = map (reverb' par_comb)

-- Original audio, [from Wikimedia](https://commons.wikimedia.org/wiki/File:Bill_Thompson_speaking.ogg):

-- > :audio ($loadaudio "Bill_Thompson_speaking.ogg")

-- With comb filter:

-- > :audio (comb 441i64 0.8f64 ($loadaudio "Bill_Thompson_speaking.ogg"))

-- Reverb:

-- > :audio (reverb_seq ($loadaudio "Bill_Thompson_speaking.ogg"))

-- > :audio (reverb_par ($loadaudio "Bill_Thompson_speaking.ogg"))

-- ==
-- entry: reverb_seq reverb_par
-- random input { [2][4410000]f64 }
-- auto output
