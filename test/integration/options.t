Each setting reports the source that won: built-in default, user file,
project file or session.

  $ root=$(cygpath -m "$PWD" 2>/dev/null || echo "$PWD")
  $ export XDG_CONFIG_HOME=$root/config APPDATA=$root/config
  $ options() { rupam repl "$@" | sed -e 's|\\|/|g' -e "s|$root|ROOT|"; }
  $ mkdir -p config/rupam analysis && cd analysis
  $ printf ':options\n:quit\n' | options --target r-dplyr
  target.language  r              session --target
  target.dialect   dplyr          session --target
  check.strict     TRUE           default
  preview.digits   2              default
  $ printf '[target]\nlanguage = "r"\ndialect = "dplyr"\n[preview]\ndigits = 4\n' > ../config/rupam/rupam.toml
  $ printf ':options\n:quit\n' | options
  target.language  r              global ROOT/config/rupam/rupam.toml
  target.dialect   dplyr          global ROOT/config/rupam/rupam.toml
  check.strict     TRUE           default
  preview.digits   4              global ROOT/config/rupam/rupam.toml
  $ printf '[preview]\ndigits = 2\n' > rupam.toml
  $ printf ':options\n:quit\n' | options
  target.language  r              global ROOT/config/rupam/rupam.toml
  target.dialect   dplyr          global ROOT/config/rupam/rupam.toml
  check.strict     TRUE           default
  preview.digits   2              project ROOT/analysis/rupam.toml
  $ printf ':options\n:quit\n' | options --target r-dplyr
  target.language  r              session --target
  target.dialect   dplyr          session --target
  check.strict     TRUE           default
  preview.digits   2              project ROOT/analysis/rupam.toml
  $ printf ':target python-polars\n:options\n:quit\n' | options --no-strict
  python-polars
  target.language  python         session :target
  target.dialect   polars         session :target
  check.strict     FALSE          session --no-strict
  preview.digits   2              project ROOT/analysis/rupam.toml
