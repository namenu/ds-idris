module Light

import Tuple
import Color

%access export

record Light where
  constructor MkLight
  position : Tuple
  intensity : Color
