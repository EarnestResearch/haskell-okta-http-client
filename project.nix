{ isProductionBuild ? false }:
let
  # Read in the Niv sources
  sources = import ./nix/sources.nix { };
  # If ./nix/sources.nix file is not found run:
  #   niv init
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Fetch the haskell.nix commit we have pinned with Niv
  haskellNix = import sources.haskellNix { };
  # If haskellNix is not found run:
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    # haskellNix.sources.nixpkgs-2009
    haskellNix.sources.nixpkgs-unstable
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    haskellNix.nixpkgsArgs;

  # Optimized builds are slower, we only want them in production.
  # Dev/CI is better off with optimizations disabled
  optimizationFlag =
    if isProductionBuild then
      "--enable-optimization"
    else
      "--disable-optimization";

  # Don't need to build tests in a production build, we only run them in CI on PRs
  enableTestsFlag =
    if isProductionBuild then "--disable-tests" else "--enable-tests";

  hsProject = pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "haskell-okta-http-client";
      src = ./.;
    };

    compiler-nix-name = "ghc8107";

    modules = [{
      packages.haskell-okta-http-client = {
        # These reduce full transitive closure size, useful for building docker containers
        enableSeparateDataOutput = true;
        dontStrip =
          false; # https://github.com/input-output-hk/haskell.nix/issues/829

        # Duplicated here, doesn't seem to take an effect otherwise. A little mystery.
        configureFlags = [ optimizationFlag ];
      };
    }];

    # Note that it passes some flags but not the others (haskell.nix auto-generates a long
    # config argument line)
    configureArgs = "-fuse-hspec-tool ${optimizationFlag} ${enableTestsFlag}";
  };
in
hsProject
