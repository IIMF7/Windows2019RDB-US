// ==UserScript==
// @name         Elm
// @match        https://package.elm-lang.org/*
// @run-at       document-start
// ==/UserScript==

let a, b, c

a = document.createElement("div")
a.onclick = () => {
  b = location.href.match(/\/packages\/([^\/]+\/[^\/]+)\//)
  location.href = "https://elm.pravdomil.com/packages/?package=" + encodeURIComponent(b ? b[1] : "")
}
a.textContent = "Open in Packages Browser"
a.style.position = "absolute"
a.style.right = "0"
a.style.top = "0"
a.style.color = "white"
a.style.padding = "8px"
a.style.fontFamily = "system-ui"
a.style.cursor = "pointer"
document.documentElement.appendChild(a)
