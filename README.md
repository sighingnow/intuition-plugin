# intuition-plugin

This plugin is a [typechecker plugin](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html#typechecker-plugins) for GHC.

This is an experiment about applying automated type-level equation reasoning techniques to guiding type
normalization in GHC.

## Why

Currently GHC is quite limited in its reasoning about arithmetic, see [Computing With Type-Level Naturals](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#typelit-tyfuns).
This plugin indented to extend the type-level arithmetic in GHC via a typechecker plugin.
Many automated strategy will be implemented and finally, this plugin should as powerful as
the automated tactics in Coq, e.g., `auto`, `intuition`, `ring` and `omega`.

## License

The MIT License (MIT), Copyright (c) 2017 HE, Tao (sighingnow@gmail.com)
