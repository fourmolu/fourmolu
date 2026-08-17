{ pkgs, ormolu, cpphs }:

let
  inherit (pkgs) lib;
  expectedFailures = [
    "brittany"
    "hlint"
    "leksah"
    "lens"
    "postgrest"
  ];
  ormolizedPackages =
    let
      ormolize = import ../nix/ormolize { inherit pkgs ormolu cpphs; };
      ormolizeOverlay = _self: _super: { };
      ormolizablePackages = pkgs.haskellPackages.override {
        overrides = ormolizeOverlay;
      };
    in
    doCheck: lib.mapAttrs
      (name: p: ormolize {
        package = p;
        inherit doCheck;
        expectedFailures =
          if lib.lists.any (x: x == name) expectedFailures
          then ./. + "/${name}.txt"
          else null;
      })
      ormolizablePackages;
in
{
  hackage = ormolizedPackages false;
  hackageTests =
    let
      ps = [
        "Agda"
        "HUnit"
        "QuickCheck"
        "ShellCheck"
        "adjunctions"
        "aeson"
        "ansi-terminal"
        "async"
        "attoparsec"
        "aws"
        "base64-bytestring"
        "bifunctors"
        "blaze-html"
        "blaze-markup"
        "brick"
        "brittany"
        "capability"
        "cassava"
        "comonad"
        "conduit"
        "contravariant"
        "criterion"
        "cryptonite"
        "diagrams-core"
        "distributed-process"
        "dlist"
        "esqueleto"
        "exceptions_0_10_12"
        "fay"
        "free"
        "hakyll"
        "hashable"
        "haxl"
        "hedgehog"
        "hledger"
        "hlint"
        "hspec-core"
        "http-client"
        "http-types"
        "idris"
        "intero"
        "leksah"
        "lens"
        "megaparsec"
        "microlens"
        "mtl_2_3_2"
        "optics"
        "optparse-applicative"
        "pandoc"
        "pandoc-types"
        "parsec3"
        "parser-combinators"
        "persistent"
        "pipes"
        "postgrest"
        "profunctors"
        "purescript"
        "raaz"
        "random"
        "recursion-schemes"
        "resourcet"
        "retry"
        "safe-exceptions"
        "scientific"
        "scotty"
        "semigroupoids"
        "servant"
        "servant-server"
        "shake"
        "split"
        "stack"
        "statistics"
        "stm_2_5_3_1"
        "swagger2"
        "tasty"
        "tensorflow"
        "text_2_1_4"
        "th-abstraction"
        "time_1_15"
        "tls"
        "transformers_0_6_3_0"
        "typed-process"
        "unliftio"
        "unordered-containers"
        "unpacked-containers"
        "uuid-types"
        "vector"
        "vector-algorithms"
        "wai"
        "warp"
        "xmonad"
        "yesod-core"
      ];
    in
    pkgs.lib.recurseIntoAttrs (lib.genAttrs ps (p: (ormolizedPackages true).${p}));
}
