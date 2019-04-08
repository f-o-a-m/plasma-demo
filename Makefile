.DEFAULT_GOAL := help

export
FINALIZED_PERIOD ?= 2

help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

#see https://stackoverflow.com/a/26936855/1798418
PATH  := node_modules/.bin:$(PATH)
SHELL := /bin/bash

build-purs: ## Build whole purescript src and test file
	pulp build --jobs 8 --src-path purs/src

compile-contracts: ## Compile all contracts from dapp/contracts and write purescript ffi modules
	rm -fr purs/src/Contracts
	sed -i '/source-dir/c\     \"source-dir\" : \"contracts\",' chanterelle.json
	chanterelle build

generate-genesis: ## Generate a cliquebait.json file
	sed -i '/source-dir/c\     \"source-dir\" : \"contracts/libraries\",' chanterelle.json
	chanterelle genesis --input ./cliquebait.json --output cliquebait-generated.json

prepare-plasma:
	sed -i "/ethereum_plasma_contract_address = /c\ethereum_plasma_contract_address = `cat abis/PlasmaMVP.json | jq \".networks[].address\"`" "$(HOME)/.plasmad/config/plasma.toml"

test-plasma:  ## Run the plasma e2e
	sed -i "/module Plasma.Contracts.PlasmaMVP where/c\module Contracts.PlasmaMVP where" purs/src/Plasma/Contracts/PlasmaMVP.purs
	pulp test --src-path purs/src --test-path purs/test -m Spec.Main

deploy-contracts: ## Deploy contracts with local config from dapp/contracts project
	chanterelle deploy ./output/Plasma.Deploy/index.js
