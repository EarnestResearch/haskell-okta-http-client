# Example shell.nix file with all optional dependencies turned on.
# This increases initial download time!
#
# cp example.shell.nix shell.nix and keep what you like.
import ./. {
  enableHoogle = true;
  enableHLS = true;
  enabledPreCommitHooks = {
    hlint.enable = true;
    hpack.enable = true;
    nixpkgs-fmt.enable = true;
    ormolu.enable = true;
    shellcheck.enable = true;
  };
  enablePureShell = true;
  customShellHook = "echo Hello!";
}
