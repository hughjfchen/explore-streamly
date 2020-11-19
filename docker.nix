{ nativePkgs ? (import ./default.nix {}).pkgs,
crossBuildProject ? import ./cross-build.nix {} }:
nativePkgs.lib.mapAttrs (_: prj:
with prj.explore-streamly;
let
  executable = explore-streamly.components.exes.explore-streamly;
  binOnly = pkgs.runCommand "explore-streamly-bin" { } ''
    mkdir -p $out/bin
    cp ${executable}/bin/explore-streamly $out/bin
    ${nativePkgs.nukeReferences}/bin/nuke-refs $out/bin/explore-streamly
  '';
in pkgs.dockerTools.buildImage {
  name = "explore-streamly";
  contents = [ binOnly pkgs.cacert pkgs.iana-etc ];
  config.Entrypoint = "explore-streamly";
}) crossBuildProject
