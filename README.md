## OpenAPI Auto-Generated Bindings to `Okta API`

Okta [API Reference](https://developer.okta.com/reference/).

Generated client code is in [gen](gen/README.md).

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

### Stack config

Please add this to your `stack.yaml` to include in your project:

```yaml
extra-deps:
- katip-0.8.2.0
- git: ssh://git@github.com/EarnestResearch/haskell-okta-http-client
  commit: "20d23c24cec1f0a12954b9b8311ffa5300c2ae07"
  subdirs:
  - gen
```

### Example

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
