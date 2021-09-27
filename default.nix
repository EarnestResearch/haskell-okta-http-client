/* Project shell configuration.

  Defaults are set to minimize INITIAL DOWNLOAD time, should be just
  enough to run 'nix-shell --command 'make test''.

  Nix-shell CLI defaults to shell.nix but falls back to default.nix.
  We leave shell.nix deliberately git ignored so that people can instantiate
  this derivation with a custom set of project-specific options that suit
  their development style.
*/
{
  # Enable if you'd like local Hoogle documentation.
  enableHoogle ? false
, # Enable if you use HLS (haskell-language-server) editor integration.
  enableHLS ? false
, /* Non-null set enables pre-commit hooks in the repository.
  See https://github.com/cachix/pre-commit-hooks.nix#getting-started

  This becomes 'hooks' value of the precommit hooks.
*/
  enabledPreCommitHooks ? null
, /* Install additional packages required to run all make targets in a nix pure shell.
  I.e. with this 'nix-shell --pure --command "make anytarget"' should work.
*/
  enablePureShell ? false
, # Append custom shell commands to the default shell hook.
  customShellHook ? ""

, # Allow inclusion of extra build inputs in shell.nix
  extraBuildInputs ? [ ]
}:
let
  hsProject = import ./project.nix { isProductionBuild = false; };
  sources = import ./nix/sources.nix { };
  haskellNix = import sources.haskellNix { };

  # HLS is provider by haskell.nix but it's missing this wrapper which emacs/etc look for
  # by default, simply proxy calls here.
  hls-wrapper =
    haskellNix.pkgs.writeShellScriptBin "haskell-language-server-wrapper" ''
      exec ${
        hsProject.tool "haskell-language-server" { }
      }/bin/haskell-language-server "$@"
    '';

  preCommitCheck = (import sources.pre-commit-hooks).run {
    src = ./.;
    # If your hooks are intrusive, avoid running on each commit with a default_states like this:
    # default_stages = ["manual" "push"];
    hooks = enabledPreCommitHooks;
  };

in
hsProject.shellFor {
  tools = {
    cabal = "latest";
    hlint = "latest";
    hspec-discover = "latest";

    # NOTE: if you add a package here not cached by upstream it will force a compilation!
    # E.g. added in buildInputs INSTEAD!
    # hpack = "latest";

  } // (if enableHLS then { haskell-language-server = "latest"; } else { })
  // (if enableHoogle then { hoogle = "latest"; } else { });

  buildInputs =
    [ haskellNix.pkgs.gnumake haskellNix.pkgs.hpack haskellNix.pkgs.niv ]
    ++ extraBuildInputs
    ++ (if enableHLS then [ hls-wrapper ] else [ ])
    ++ (if enablePureShell then [
      haskellNix.pkgs.cacert
      haskellNix.pkgs.git
      haskellNix.pkgs.nix
      haskellNix.pkgs.curl
      haskellNix.pkgs.jq
    ] else
      [ ]);

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  #
  # Note: this interferes with hspec-discover tool
  #       https://github.com/input-output-hk/haskell.nix/issues/231
  #       Our work-around is to use "use-hspec-tool" cabal flag
  #
  exactDeps = true;

  withHoogle = enableHoogle;

  shellHook = ''
    ${if enabledPreCommitHooks == null then "" else preCommitCheck.shellHook}
    ${customShellHook}
  '';
}
