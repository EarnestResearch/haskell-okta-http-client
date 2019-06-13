.DEFAULT_GOAL=help

# (Slightly) Patched version of the generated client under version control
VC_GEN:=gen

# Temp files
TARGET:=target

# Unzipped downloaded client
TARGET_CLIENT:=$(TARGET)/haskell-http-client-client

HASKELL_API_GEN:=https://api.openapi-generator.tech/api/gen/clients/haskell-http-client
OKTA_API_SPEC:=https://raw.githubusercontent.com/okta/okta-sdk-golang/master/openapi/spec.json

help: ## help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'


build: ## build generated client
	cd $(VC_GEN) && stack build


test: ## run bundled unit tests
	cd $(VC_GEN) && stack test

# Move generated code to version controlled dir
generate: ## regenerates client from api spec
	rm -rf $(TARGET) $(VC_GEN)
	mkdir -pv $(TARGET)
	curl -X POST --header 'Content-Type: application/json' \
		--header 'Accept: application/json' \
		-d '{"openAPIUrl": "$(OKTA_API_SPEC)"}' \
		'$(HASKELL_API_GEN)' | jq -r .link | xargs curl -o $(TARGET)/client.zip
	cd $(TARGET) && unzip client.zip
	rm -f $(TARGET_CLIENT)/git_push.sh
	cd $(TARGET_CLIENT) && patch -p2 -i ../../patch/stack.yaml.patch
	cd $(TARGET_CLIENT) && patch -p2 -i ../../patch/expose-full-profile.patch
	mv -v $(TARGET_CLIENT) $(VC_GEN)


clean: ## clean work dir
	rm -rf $(TARGET)


clobber: ## remove everything not tracked by git
	git clean -fdx .
