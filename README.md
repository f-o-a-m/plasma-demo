# plasma-demo

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

### run test suite
```bash
> make test-plasma
```
