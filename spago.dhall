{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-fluent-design"
, dependencies = [ "console", "css", "effect", "prelude", "psci-support", "halogen" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
