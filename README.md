# plasma-demo

## Setting up the tendermint node and server
1. follow the installation instructions here https://github.com/f-o-a-m/plasma-mvp-sidechain/tree/http-server. This means running `go install` in both the server and client directories (again, see the instructions there).
2. Running `plasmad init` inside that directory should create `.plasmad` rooted in `$HOME`. Inside you should find a `config/plasma.toml` file that's important for the next part below.
3. To start the node, run `$GOBIN/plasmad start`. Note that in order to start the node, you need to have already deployed a contract and have put the address in this config file. See below for details.
4. To start the server, run `$GOBIN/plasmacli rest-server`. This should spin up the server on `127.0.0.1:1317`.

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
