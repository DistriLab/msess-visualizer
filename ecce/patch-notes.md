# haskeline
Remove `haskeline` from `ecce`, re-implement `haskeline` dependency in another 
library called `ecce-haskeline`.

This is so that the `ghcjs` compiler can compile without depending on 
`haskeline`.
