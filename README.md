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

## Heroku Review

[Heroku Review](https://devcenter.heroku.com/articles/github-integration-review-apps)
is enabled on this application. It means when a PR is sent then heroku
automatically deploys an application for that branch.


## About BigBinary

![BigBinary](https://raw.githubusercontent.com/bigbinary/bigbinary-assets/press-assets/PNG/logo-light-solid-small.png?raw=true)

ApiSnapshot is maintained by [BigBinary](https://www.BigBinary.com). BigBinary is a software consultancy company. We build web and mobile applications using Ruby on Rails, React.js, React Native and Elm.
