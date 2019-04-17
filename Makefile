.DEFAULT_GOAL := help

export
FINALIZED_PERIOD ?= 3
NODE_URL ?= http://localhost:8545

# plasma config vars, need to supply operator private key
PLASMA_CONFIG_DESTINATION ?= ./plasma.toml
IS_OPERATOR ?= false
COMMITMENT_RATE ?= 2
PLASMA_ARTIFACT ?= ./abis/PlasmaMVP.json

help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

#see https://stackoverflow.com/a/26936855/1798418
PATH  := node_modules/.bin:$(PATH)
SHELL := /bin/bash

install: ## Sets up prerequistes
	npm install && bower install

build-purs: compile-contracts ## Build purescript src files
	pulp build --jobs 8 --src-path purs/src

build-purs-editor: ## Build purescript src and test files for using VSCode
	pulp build --jobs 8 --src-path purs/src -I purs/test -- --json-errors

compile-contracts: ## Compile all contracts from dapp/contracts and write purescript ffi modules
	rm -fr purs/src/Contracts
	pulp build --modules Plasma.Deploy; chanterelle build

generate-genesis: ## Generate a cliquebait.json file
	chanterelle genesis --input ./cliquebait.json --output cliquebait-generated.json

write-plasma-toml: ## write the plasma config to the plasma.toml file
	pulp run --jobs 8 --src-path purs/src -m Plasma.Config.TOMLMain

test-plasma:  ## Run the plasma e2e
	NODE_URL=$(NODE_URL) pulp test --src-path purs/src --test-path purs/test -m Spec.Main

deploy-contracts: compile-contracts ## Deploy contracts with local config from dapp/contracts project
	NODE_URL=$(NODE_URL) chanterelle deploy ./output/Plasma.Deploy/index.js

deploy-and-test: deploy-contracts write-plasma-toml
	sleep 2
	docker exec -d -e NODE_URL='"http://cliquebait:8545"'  plasma-demo_plasma_1 './run.sh'
	sleep 2
	NODE_URL="http://localhost:8545" PLASMA_ADDRESS=`cat abis/PlasmaMVP.json | jq -r ".networks[].address"` make test-plasma

frontend-copy-assets: ## Copy all static assets into `dist` folder
	mkdir -p frontend/dist && cp -R frontend/static/** frontend/dist

frontend-start: ## Starts webserver with livereload. Note: You have  want to build all purescript sources first)
	webpack-dev-server --port 3333 --hot

frontend-build: ## Builds css html and js assets.
	webpack
