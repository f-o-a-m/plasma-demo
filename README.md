# plasma-demo

## Requirements
- [npm](https://www.npmjs.com/)
- [Go](https://golang.org/doc/install)
- Set [GOPATH environment variable](https://github.com/golang/go/wiki/SettingGOPATH)
- [dep](https://golang.github.io/dep/docs/installation.html)

## Setting up `plasma-mvp-sidechain` (only once)

1. Install original sources of `plasma-mvp-sidechain` and switch to our custom branch as follow:
```
go get github.com/FourthState/plasma-mvp-sidechain

cd $GOPATH/src/github.com/FourthState/plasma-mvp-sidechain

git remote rename origin upstream
git remote add origin git@github.com:f-o-a-m/plasma-mvp-sidechain.git

git fetch --all
git checkout http-server

dep ensure -vendor-only
```

2. Install `plasmad` and `plasmacli`
```
cd server/plasmad
go install ./...
```
```
cd client/plasmacli
go install ./...
```

## Run the tendermint node and server

1. Run `plasmad init` should create `.plasmad` rooted in `$HOME`. Inside you should find a `config/plasma.toml` file. Open that file to add the address of the `PlasmaMVP` contract to `ethereum_plasma_contract_address`. To do that, you need to have already deployed the `PlasmaMVP` contract as described in [compile contracts](#compile-contracts).
2. To start the node, run `plasmad start`.
3. To start the server, run `plasmacli rest-server`. This should spin up the server on `127.0.0.1:1317`.

## Running the test suite

### Install
```bash
> npm install
```

### build purs files
```bash
> make build-purs
```

### generating cliquebait.json and launching cliquebait

Due to a bug in the way chanterelle is trying to resolve important statements, you will need to temporarily change
the `source-dir` field in `chanterelle.json` to `contracts/libraries`. Then you can run

```bash
> make generate-genesis
```

You can then launch cliquebait with this json file using
```bash
>  docker run  -it -p 8545:8545  -v `pwd`/cliquebait-generated.json:/cliquebait/cliquebait.json foamspace/cliquebait:latest
```

### compile contracts

Make sure that the previous change in `chanterelle.json` has been reverted, so that the `source-dir` field is set to `contracts`.

```bash
> make compile-contracts
```

### deploy contracts
```bash
make deploy-contracts
```

### run test suite
Make sure you have the tendermint node up
```bash
> PLASMA_ADDRESS=<whatever-the-address-is> make test-plasma
```

`PLASMA_ADDRESS` is visible after running `plasmad start`. In following example it is `0xe545eaf693277ead76f5d9b4665291b0ac38853c`:
```
plasmad start 
I[2046-04-02|14:05:54.360] Starting ABCI with Tendermint                module=main 
I[2046-04-02|14:05:54.388] binding to contract address 0xe545eaf693277ead76f5d9b4665291b0ac38853c module=main

...

```

## WARNINGS
We are still trying to figure out how to automate config/setup so that it's the same everywhere (test config vs $HOME/.plasmad/config/plasma.toml etc). Until that's done, you should check this warnings list/ update it with new warnings when you find them.
1. When running `make plasma-test` you need to supply the root chain plasma contract with environment variable `PLASMA_ADDRESS`.
2. When running `make plasma-test` you need to make sure that the `FINALIZE_PERIOD` environment variable is the same as what's listed under `ethereum_finality` in the plasma.toml file. Otherwise you will not be able to pass the include deposit test.
