build-purs: ## Build whole purescript src and test file
	pulp build --jobs 8 --src-path purs/src -I purs/test

compile-contracts: ## Compile all contracts from dapp/contracts and write purescript ffi modules
	rm -fr purs/src/Contracts
	node chanterelle.js compile
	node chanterelle.js codegen

generate-genesis: ## Generate a cliquebait.json file
	node chanterelle.js genesis --input ./cliquebait.json --output cliquebait-generated.json

test-plasma:  ## Run the plasma e2e
	pulp test --src-path purs/src --test-path purs/test -m Spec.Main

deploy-contracts: ## Deploy contracts with local config from dapp/contracts project
	pulp run --jobs 8 --src-path purs/src -m Plasma.Deploy
