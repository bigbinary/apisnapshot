# ApiSnapshot


## Setup

```
cp config/database.yml.postgresql config/database.yml
./bin/bundle install
./bin/rails db:setup
./bin/yarn install
npm install -g elm
elm-package install
```


## Local Development

```
npm start
```

Once it shows `webpack: Compiled successfully.` message in the terminal,
visit the app at http://localhost:3333.

Webpack will automatically compile if a file inside `app/javascript/` directory is modified.
