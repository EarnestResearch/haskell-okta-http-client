.DEFAULT_GOAL=help

# (Slightly) Patched version of the generated client under version control
VC_GEN:=gen

# Temp files
TARGET:=target

# Unzipped downloaded client
TARGET_CLIENT:=$(TARGET)/haskell-http-client-client

HASKELL_API_GEN:=https://api.openapi-generator.tech/api/gen/clients/haskell-http-client
OKTA_API_SPEC:=https://raw.githubusercontent.com/okta/okta-management-openapi-spec/master/resources/spec.yaml

help: ## help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'

build: ## run bundled unit tests
	cabal build all

test: ## run bundled unit tests
	cabal test all

generate: clean generate-openapi patch-generated ## regenerates client from api spec

generate-openapi: $(TARGET)/client.zip
	rm -rf $(VC_GEN)
	cd $(TARGET) && unzip client.zip
	rm -f $(TARGET_CLIENT)/git_push.sh
	mv -v $(TARGET_CLIENT) $(VC_GEN)

$(TARGET)/client.zip:
	mkdir -pv $(TARGET)
	curl -X POST \
		--header 'Content-Type: application/json' \
		--header 'Accept: application/json' \
		-d '{"openAPIUrl": "$(OKTA_API_SPEC)"}' \
		'$(HASKELL_API_GEN)' | jq -r .link | xargs curl -o $(TARGET)/client.zip

patch-generated:
	patch -p1 -i patch/expose-user-groupids.patch
	patch -p1 -i patch/expose-full-profile.patch
	patch -p1 -i patch/tests.patch
	patch -p1 -i patch/query-param-mappings.patch

clean: ## clean work dir
	rm -rf $(TARGET)

clobber: ## remove everything not tracked by git
	git clean -fdx .
