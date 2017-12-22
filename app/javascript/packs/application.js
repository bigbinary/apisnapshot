// Run this example by adding <%= javascript_pack_tag "elm_root" %> to the
// head of your layout file, like app/views/layouts/application.html.erb.
// It will render "Hello Elm!" within the page.

import 'bootstrap'
import './application.css'
import Elm from '../Main'

document.addEventListener('DOMContentLoaded', () => {
  const target = document.createElement('div')

  document.body.appendChild(target)
  Elm.Main.embed(target)
})
