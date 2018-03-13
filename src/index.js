if (window.navigator.serviceWorker) {
  window.navigator.serviceWorker
    .register("/sw.js")
    .then(registration => {
      console.log(registration);

      const Elm = require("./Main.elm");

      const app = Elm.Main.embed(document.body, localStorage.getItem("DATA"));

      app.ports.saveAuth.subscribe(data => localStorage.setItem("DATA", data));

      app.ports.clearAuth.subscribe(() => localStorage.removeItem("DATA"));

      app.ports.log.subscribe(console.log);
    })
    .catch(alert);
} else {
  alert("service worker necessary");
}
