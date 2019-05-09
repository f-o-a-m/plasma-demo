# 🧼🔥 plasma-demo 🔥🧼
[![Build Status](https://travis-ci.com/f-o-a-m/plasma-demo.svg?token=S5ycohXqQwQa9vbXx9fF&branch=master)](https://travis-ci.com/f-o-a-m/plasma-demo)

## Requirements
- [npm](https://www.npmjs.com/)
- [Go](https://golang.org/doc/install)
- Set [GOPATH environment variable](https://github.com/golang/go/wiki/SettingGOPATH)
- [dep](https://golang.github.io/dep/docs/installation.html)

### If you are using `nix`

```shell 
nix-shell -p nodejs-8_x go dep jq 
```

You wold also need to add this to `~/.profile`:
```
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
```

## Easy install


We now provide a super simple way to run the test-suite. It only depends on docker!

```
make install
docker-compose pull
docker-compose up -d
sleep 5
make build-purs
make deploy-and-test
docker-compose down
```

If you want to run each step manually, you can do something like but note that this currently deploys the contracts twice
```
make install
docker-compose pull
docker-compose up -d cliquebait
make deploy-contracts
make write-plasma-toml
sleep 5
docker-compose up -d plasma
make build-purs
make deploy-and-test
docker-compose down
```


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

```bash
> make generate-genesis
```

You can then launch cliquebait with this json file using
```bash
>  docker run  -it -p 8545:8545  -v `pwd`/cliquebait-generated.json:/cliquebait/cliquebait.json foamspace/cliquebait:latest
```

### compile contracts


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

## Generating a plasma.toml config file
You can generate a plasma.toml config file by running `make write-plasma-toml`. If you would like to create a config for an `Operator`, set `IS_OPERATOR=true OPERATOR_PRIVATE_KEY=<whatever-it-is> make write-plasma-toml`.

## WARNINGS
We are still trying to figure out how to automate config/setup so that it's the same everywhere (test config vs $HOME/.plasmad/config/plasma.toml etc). Until that's done, you should check this warnings list/ update it with new warnings when you find them.
1. When running `make plasma-test` you need to supply the root chain plasma contract with environment variable `PLASMA_ADDRESS`.
2. When running `make plasma-test` you need to make sure that the `FINALIZED_PERIOD` environment variable is the same as what's listed under `ethereum_finality` in the plasma.toml file. Otherwise you will not be able to pass the include deposit test.
3. This part is confusing https://www.learnplasma.org/en/learn/mvp.html#confirmation-signatures
