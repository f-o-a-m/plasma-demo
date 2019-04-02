.DEFAULT_GOAL := help

export
FINALIZED_PERIOD ?= 2

help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'


build-purs: ## Build whole purescript src and test file
	pulp build --jobs 8 --src-path purs/src -I purs/test

compile-contracts: ## Compile all contracts from dapp/contracts and write purescript ffi modules
	rm -fr purs/src/Contracts
	node chanterelle.js compile
	node chanterelle.js codegen

generate-genesis: ## Generate a cliquebait.json file
	node chanterelle.js genesis --input ./cliquebait.json --output cliquebait-generated.json

test-plasma:  ## Run the plasma e2e
	ulp test --src-path purs/src --test-path purs/test -m Spec.Main

deploy-contracts: ## Deploy contracts with local config from dapp/contracts project
	pulp run --jobs 8 --src-path purs/src -m Plasma.Deploy
