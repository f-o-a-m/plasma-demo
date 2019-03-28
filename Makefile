build-purs: ## Build whole purescript src and test file
	pulp build --jobs 8 --src-path purs/src

compile-contracts: ## Compile all contracts from dapp/contracts and write purescript ffi modules
	rm -fr purs/src/Contracts
	node chanterelle.js compile
	node chanterelle.js codegen

test-plasma:  ## Run the plasma e2e
	pulp test --src-path purs/src --test-path purs/test -m Spec.Main
