## OpenAPI Auto-Generated Bindings to `Okta API`

Haskell environment is assumed to be available [see ghcup](https://www.haskell.org/ghcup/).

Known to compile with

``` text
$ ghcup list -c set -r
ghc 9.2.2 base-4.16.1.0 hls-powered
cabal 3.6.2.0 latest,recommended 
hls 1.7.0.0 latest,recommended 
stack 2.7.5 latest,recommended 
ghcup 0.1.17.8 latest,recommended 
```


Okta [API Reference](https://developer.okta.com/reference/).

Generated client code is in [gen](gen/README.md), a small [RIO](https://haskell.fpcomplete.com/library/rio) wrapper is in [rio](rio).

To re-generate please run

```bash
# make generate
```

Run `make help` to see all make targets.

### Gotchas

Note that you need to use `newTlsManager` or otherwise configure your client
with TLS or requests will fail.

Also note that API token you get from Okta **MUST BE PREFIXED** with `SSWS ` when
setting up auth method with `AuthApiKeyApiToken`.

### Auto-generated API

#### Stack config

Please add this to your `stack.yaml` to include in your project:

```yaml
extra-deps:
- katip-0.8.2.0
- git: ssh://git@github.com/EarnestResearch/haskell-okta-http-client
  commit: "27ca224cba36bbbe25714e89395671f32b81034b"
  subdirs:
  - gen
```

#### Example

A complete GHCI example that lists groups:

```haskell
import Data.Monoid
import Network.HTTP.Client.TLS
import Okta

let myOktaHost = "https://SOMETHING.okta.com"
let myApiToken = "APITOKEN"


mgr <- newTlsManager
config0 <- withStderrLogging =<< newConfig 

let config =
      config0 { configHost = myOktaHost }
        `addAuthMethod` AuthApiKeyApiToken ("SSWS " <> myApiToken)

lgrRes <- dispatchMime mgr config listGroups
```

Note you can use

```
:{
multi line
code
:}
```

REPL syntax to paste into `ghci` (run `stack repl` under `gen`).


### RIO wrapper

Loads Okta config / HTTP manager from environment, adds pagination to generated APIs with pipes.
A small complete example is found in [rio/example](rio/example).


#### Stack config

Please add this to your `stack.yaml` to include both, auto-generated API as well as RIO wrapper in your project:

```yaml
extra-deps:
- github: EarnestResearch/haskell-okta-http-client
  commit: "27ca224cba36bbbe25714e89395671f32b81034b"
  subdirs:
  - gen
  - rio
```
