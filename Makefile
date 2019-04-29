.DEFAULT_GOAL := help

export
FINALIZED_PERIOD ?= 3
NODE_URL ?= http://localhost:8545

# plasma config vars, need to supply operator private key
PLASMA_CONFIG_DESTINATION ?= ./plasma.toml
IS_OPERATOR ?= true
OPERATOR_PRIVATE_KEY ?= e1c01c07784956abe9c72eb20ac6f0a075edb3e0f61e833e0855a52c6e7c7037
COMMITMENT_RATE ?= 2
PLASMA_ARTIFACT ?= ./abis/PlasmaMVP.json

help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

#see https://stackoverflow.com/a/26936855/1798418
PATH  := node_modules/.bin:$(PATH)
SHELL := /bin/bash

install: ## Sets up prerequistes
	npm install && bower install

build-purs: ## Build purescript src files
	pulp build --jobs 8

build-purs-editor: ## Build purescript src and test files for using VSCode
	pulp build --jobs 8 -I test -- --json-errors

compile-contracts: ## Compile all contracts from dapp/contracts and write purescript ffi modules
	rm -fr src/Contracts
	pulp build -I local/Plasma --modules Plasma.Deploy; chanterelle build

generate-genesis: ## Generate a cliquebait.json file
	chanterelle genesis --input ./cliquebait.json --output cliquebait-generated.json

write-plasma-toml: ## write the plasma config to the plasma.toml file
	pulp run --jobs 8 --src-path local/Plasma/Config -m Plasma.Config.TOMLMain

test-plasma:  ## Run the plasma e2e
	OPERATOR_PRIVATE_KEY=e1c01c07784956abe9c72eb20ac6f0a075edb3e0f61e833e0855a52c6e7c7037 \
	NODE_URL=$(NODE_URL) pulp test --test-path test -m Spec.Main

deploy-contracts: compile-contracts ## Deploy contracts with local config from dapp/contracts project
	pulp build -I src --src-path local/Plasma --modules Plasma.Deploy; NODE_URL=$(NODE_URL) chanterelle deploy ./output/Plasma.Deploy/index.js

deploy-and-test: deploy-contracts write-plasma-toml
	sleep 2
	docker exec -d -e NODE_URL='"http://cliquebait:8545"'  plasma-demo_plasma_1 './run.sh'
	sleep 2
	NODE_URL="http://localhost:8545" PLASMA_ADDRESS=`cat abis/PlasmaMVP.json | jq -r ".networks[].address"` make test-plasma
