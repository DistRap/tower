let haskellCi =
      https://raw.githubusercontent.com/sorki/github-actions-dhall/pending/haskell-ci.dhall

in    haskellCi.generalCi
        haskellCi.matrixSteps
        ( Some
            { ghc =
              [ haskellCi.GHC.GHC8107
              , haskellCi.GHC.GHC902
              , haskellCi.GHC.GHC924
              ]
            , cabal = [ haskellCi.Cabal.Cabal36 ]
            }
        )
    : haskellCi.CI.Type
