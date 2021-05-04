let base = ./spago.dhall

in    base
    ⫽ { sources =
          base.sources # [ "test/**/*.purs" ]
      }

