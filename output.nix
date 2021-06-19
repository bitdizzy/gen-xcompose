{ gen-xcompose, runCommand }:

runCommand "XCompose" {
    nativeBuildInputs = [ gen-xcompose ];
  } ''
    export LANG=C.UTF-8
    export LC_ALL=C.UTF-8
    gen-xcompose > $out
  ''

