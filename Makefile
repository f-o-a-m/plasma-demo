.DEFAULT_GOAL := help

export
FINALIZED_PERIOD ?= 18
NODE_URL ?= http://localhost:8545

# plasma config vars, need to supply operator private key
PLASMA_CONFIG_DESTINATION ?= $(HOME)/.plasmad/config/plasma.toml
IS_OPERATOR ?= true
COMMITMENT_RATE ?= 2
PLASMA_ARTIFACT ?= ./abis/PlasmaMVP.json

help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

#see https://stackoverflow.com/a/26936855/1798418
PATH  := node_modules/.bin:$(PATH)
SHELL := /bin/bash

install: ## Sets up prerequistes
	npm install && bower install

build-purs: ## Build whole purescript src and test file
	pulp build --jobs 8 --src-path purs/src

compile-contracts: build-purs ## Compile all contracts from dapp/contracts and write purescript ffi modules
	rm -fr purs/src/Contracts
	chanterelle build

generate-genesis: ## Generate a cliquebait.json file
	chanterelle genesis --input ./cliquebait.json --output cliquebait-generated.json

write-plasma: ## write the plasma config to the plasma.toml file
	pulp run --jobs 8 --src-path purs/src -m Plasma.Config.TOMLMain


prepare-plasma:
	sed -i "/ethereum_plasma_contract_address = /c\ethereum_plasma_contract_address = `cat abis/PlasmaMVP.json | jq \".networks[].address\"`" "$(HOME)/.plasmad/config/plasma.toml"

test-plasma:  ## Run the plasma e2e
	NODE_URL=$(NODE_URL) pulp test --src-path purs/src --test-path purs/test -m Spec.Main

deploy-contracts: compile-contracts ## Deploy contracts with local config from dapp/contracts project
	NODE_URL=$(NODE_URL) chanterelle deploy ./output/Plasma.Deploy/index.js

deploy-and-test: deploy-contracts
	sleep 2
	docker exec -d -e NODE_URL='"http://cliquebait:8545"' -e PLASMA_ADDRESS=`cat abis/PlasmaMVP.json | jq -r ".networks[].address"` plasma-demo_plasma_1 './run.sh'
	sleep 2
	NODE_URL="http://localhost:8545" PLASMA_ADDRESS=`cat abis/PlasmaMVP.json | jq -r ".networks[].address"` make test-plasma
