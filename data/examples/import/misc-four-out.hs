import A hiding (
    foobarbazqux,
 )

import Name hiding ()

import {-# SOURCE #-} safe qualified Module as M hiding (a, b, c, d, e, f)
