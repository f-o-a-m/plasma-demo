.DEFAULT_GOAL := help

export
FINALIZED_PERIOD ?= 2

help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'


build-purs: ## Build whole purescript src and test file
	node_modules/.bin/pulp build --jobs 8 --src-path purs/src -I purs/test

compile-contracts: ## Compile all contracts from dapp/contracts and write purescript ffi modules
	rm -fr purs/src/Contracts
	node_modules/.bin/chanterelle build

generate-genesis: ## Generate a cliquebait.json file
	node_modules/.bin/chanterelle genesis --input ./cliquebait.json --output cliquebait-generated.json

test-plasma:  ## Run the plasma e2e
	node_modules/.bin/pulp test --src-path purs/src --test-path purs/test -m Spec.Main

deploy-contracts: ## Deploy contracts with local config from dapp/contracts project
	node_modules/.bin/chanterelle deploy ./output/Plasma.Deploy/index.js
