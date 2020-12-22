import { Elm } from "./Main.elm";

// Get all elm nodes
const elmNodes = document.querySelectorAll(".elm");

// Initialize Elm on each elmNode
elmNodes.forEach((node) => {
  Elm.Main.init({
    node,
    flags: getFlags(node.dataset.flags),
  });
});

// Parse the JSON from IHP or return null if there is none
function getFlags(data) {
  return data ? JSON.parse(data) : null;
}