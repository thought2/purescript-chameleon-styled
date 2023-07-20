{ name = "virtual-dom-styled"
, dependencies =
  [ "arrays"
  , "foldable-traversable"
  , "maybe"
  , "newtype"
  , "prelude"
  , "strings"
  , "tuples"
  , "unordered-collections"
  , "virtual-dom"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
