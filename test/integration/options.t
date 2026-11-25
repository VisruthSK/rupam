Each setting reports the source that won: built-in default, user file,
project file or session.

  $ export XDG_CONFIG_HOME=$PWD/config APPDATA=$PWD/config
  $ mkdir -p config/rupam analysis && cd analysis
  $ printf ':options\n:quit\n' | rupam repl
  target.dialect   unset          default
  check.strict     true           default
  preview.digits   2              default
  $ printf '[target]\ndialect = "r-dplyr"\n[preview]\ndigits = 4\n' > ../config/rupam/rupam.toml
  $ printf ':options\n:quit\n' | rupam repl
  target.dialect   r-dplyr        global $TESTCASE_ROOT/config/rupam/rupam.toml
  check.strict     true           default
  preview.digits   4              global $TESTCASE_ROOT/config/rupam/rupam.toml
  $ printf '[preview]\ndigits = 2\n' > rupam.toml
  $ printf ':options\n:quit\n' | rupam repl
  target.dialect   r-dplyr        global $TESTCASE_ROOT/config/rupam/rupam.toml
  check.strict     true           default
  preview.digits   2              project $TESTCASE_ROOT/analysis/rupam.toml
  $ printf ':options\n:quit\n' | rupam repl --target r-dplyr
  target.dialect   r-dplyr        session --target
  check.strict     true           default
  preview.digits   2              project $TESTCASE_ROOT/analysis/rupam.toml
  $ printf ':target python-polars\n:options\n:quit\n' | rupam repl --no-strict
  target.dialect   python-polars  session :target
  check.strict     false          session --no-strict
  preview.digits   2              project $TESTCASE_ROOT/analysis/rupam.toml
