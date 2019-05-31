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

### Example

Note you can use

```
:{
multi
line
code
:}
```

REPL syntax to paste into `ghci` (run `stack repl` under `gen`).

A complete example that lists groups:

```haskell
--import Network.HTTP.Client

import Data.Monoid
import Network.HTTP.Client.TLS
import Okta

let myOktaHost = "https://SOMETHING.okta.com"
let myApiToken = "APITOKEN"


mgr <- newTlsManager
config0 <- withStderrLogging =<< newConfig 

let config = config0 { configHost = myOktaHost } `addAuthMethod` (AuthApiKeyApiToken ("SSWS " <> myApiToken))

lgrRes <- dispatchMime mgr config listGroups
```
