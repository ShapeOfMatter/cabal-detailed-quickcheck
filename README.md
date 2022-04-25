# Turn QuickCheck properties into tests for Cabal

This is a library for using [QuickCheck](https://hackage.haskell.org/package/QuickCheck) properties in Cabal’s `detailed-0.9` test interface ([docs](https://cabal.readthedocs.io/en/3.6/cabal-package.html#example-package-using-detailed-0-9-interface)).

You can construct a `PropertyTest` for your property and call `getPropertyTest` on it. For more details, see the Haddock documentation on [Hackage](https://hackage.haskell.org/package/cabal-detailed-quickcheck).

This project is developed on GitHub: <https://github.com/schuelermine/cabal-detailed-quickcheck>
