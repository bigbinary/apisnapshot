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
