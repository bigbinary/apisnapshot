// This import loads the firebase namespace along with all its type information
import * as firebase from 'firebase/app';

// This imports load individual 'database' service into the firebase namespace
import 'firebase/database';

import './main.css';
import { Main } from './Main.elm';

const app = Main.embed(document.getElementById('root'));

app.ports.localStorageGet.subscribe(key => {
  app.ports.localStorageGetResponse.send(localStorage[key] || "{}")
});

app.ports.localStorageSet.subscribe(object => {
  try {
    localStorage[object.key] = object.value;
    app.ports.localStorageSetResponse.send("true");
  } catch (error) {
    console.error(`Error while setting => localStorage[${key}] = ${value}`, error);
    app.ports.setResponse.send("false");
  }
});


app.ports.firebaseInitialize.subscribe(configString => {
  const config = JSON.parse(configString);

  const initializeApp = () => {
    firebase.initializeApp(config);

    try {
      const database = firebase.database();
      app.ports.firebaseInitializeResponse.send({ "success": true, "error": "" });
    } catch (error) {
      app.ports.firebaseInitializeResponse.send({ "success": false, "error": error.message });
    }
  }

  // Fix: Firebase App named '[DEFAULT]' already exists (app/duplicate-app)
  if (firebase.apps.length > 0) {
    firebase.app().delete().then(initializeApp);
  } else {
    initializeApp();
  }
});
