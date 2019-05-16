FROM node:8

WORKDIR /usr/src/app

COPY . .

RUN npm install
RUN ./node_modules/.bin/bower --allow-root install
RUN make build-purs
RUN make compile-contracts

CMD echo hello
