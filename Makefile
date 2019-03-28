build-purs: ## Build whole purescript src and test file
	pulp build --jobs 8 --src-path purs/src

compile-contracts: ## Compile all contracts from dapp/contracts and write purescript ffi modules
	rm -fr purs/src/Contracts
	pulp run --jobs 8 --src-path purs/src/DApp/Compile -m DApp.Compile.Main
