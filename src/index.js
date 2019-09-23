import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

// import "../public/pokemons/1.png";
// import "../public/pokemons/2.png";
// import "../public/pokemons/3.png";
// import "../public/pokemons/4.png";
// import "../public/pokemons/5.png";
// import "../public/pokemons/6.png";
// import "../public/pokemons/7.png";
// import "../public/pokemons/8.png";
// import "../public/pokemons/9.png";
// import "../public/pokemons/10.png";
// import "../public/pokemons/11.png";
// import "../public/pokemons/12.png";
// import "../public/pokemons/13.png";
// import "../public/pokemons/badge31.png";
// import "../public/pokemons/pokeball31.png";
// import "../public/bigIcon.png";
// import "../public/favicon2.png";
// import "../public/logo-yellow.svg";

Elm.Main.init({
  node: document.getElementById("root")
});

registerServiceWorker();
