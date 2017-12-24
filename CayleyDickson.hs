module CayleyDickson(i) where

data CayleyDickson = Node Float | Branch CayleyDickson CayleyDickson Integer
  deriving (Eq)

uniformization :: CayleyDickson -> CayleyDickson
uniformization (Node r) = Node r
uniformization (Branch real imag h) =
  case imag of
    Node 0 -> real
    _ -> Branch real imag h

height :: CayleyDickson -> Integer
height (Node _) = 0
height (Branch real imag h)  = h


i :: Integer -> CayleyDickson
i 0 = Node 1.0
i 1 = Branch (Node 0.0) (Node 1.0) 1
i n = 
  let
    (l,r) =
      let 
        logAndReminder logarithm powerOfTwo =
          let newPower = 2 * powerOfTwo
          in
            if n >= newPower
              then logAndReminder (logarithm + 1) newPower
              else (logarithm, n - powerOfTwo)
      in
        logAndReminder 0 1
    imag = i r
  in
    Branch (Node 0.0) imag (l + 1)

instance Show CayleyDickson where
  show (Node r) = show r
  show cayleyDickson =
    let
      showImaginary n (Node r) =
        if r == 0.0
          then ""
          else if n == 0
            then show r
            else show r ++ "i_" ++ show n
      showImaginary n (Branch real imag h) =
        let
          low = h - 1
        in
          let
            left = showImaginary n real
          in
            if left == ""
              then showImaginary (n + 2^low) imag
              else left ++ " + " ++ showImaginary (n + 2^low) imag
    in
      showImaginary 0 cayleyDickson

realPart :: CayleyDickson -> CayleyDickson
realPart (Node r) = (Node r)
realPart (Branch r _ _) = r

imagPart :: CayleyDickson -> CayleyDickson
imagPart (Node r) = (Node 0.0)
imagPart (Branch _ l _) = l

negate :: CayleyDickson -> CayleyDickson
negate (Node r) = Node (- r)
negate (Branch l r h) = Branch (CayleyDickson.negate l) 
                               (CayleyDickson.negate r)
                               h

conjugate :: CayleyDickson -> CayleyDickson
conjugate (Node r) = (Node r)
conjugate (Branch l r h) = Branch (conjugate l)
                                  (CayleyDickson.negate r)
                                  h

squareSum :: CayleyDickson -> Float
squareSum (Node r) = r * r
squareSum (Branch l r _) = (squareSum l) + (squareSum r)

absoluteValue :: CayleyDickson -> Float
absoluteValue = sqrt . squareSum 

instance Num CayleyDickson where
  (Node l) + (Node r) = Node (l + r)
  l + r =
    let
      hl = height l
      hr = height r
      d = hl - hr
    in
      if d == 0 then uniformization $ Branch (realPart l + realPart r) 
                                             (imagPart l + imagPart r) 
                                             hl
      else if d > 0 then uniformization $ Branch (realPart l + r)
                                                 (imagPart l)
                                                 hl
      else uniformization $ Branch (l + realPart r)
                                   (imagPart r)
                                   hr

  l - r = l + CayleyDickson.negate r

  (Node l) * (Node r) = Node (l * r)
  l * r = 
    let
      hl = height l
      hr = height r
      d = hl - hr
    in
      if d == 0 
        then
          let
            p = realPart l
            q = imagPart l
            s = realPart r
            u = imagPart r
          in
            uniformization $
              Branch ((p * s) - (conjugate u * q))
                     ((u * p) + (q * conjugate s))
                     hl
      else if d > 0 then uniformization $ Branch (realPart l * r)
                                                 (imagPart l * conjugate r)
                                                 hl
      else uniformization $ Branch (l * realPart r)
                                   (imagPart r  * l)
                                   hr

  abs = Node . absoluteValue

  fromInteger = Node . fromInteger

  signum _ = Node 0
