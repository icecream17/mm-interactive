import { cDecl, vDecl } from "./hmm.js"
import { make } from "./html.js"


export const main = make("div")
main.style.borderTop = "2px solid green"
main.style.borderBottom = "2px solid green"

/** @type {HTMLButtonElement} */
export const doAct = make("button", "start scope")
doAct.type = "button"


// $cv //
// Have $c and $v share some elements
const $cvin = make("input")
$cvin.type = "text"
// $cvin.pattern = "^[a-zA-Z0-9!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~]+$"
$cvin.placeholder = "Printable ASCII, e.g. <->, space separate to enter multiple values at once"

const $cvt = document.createTextNode("New ?: ")
const $cvl = make("label", $cvt, $cvin)


// $c //
export function $csetup() {
    $cvt.textContent = "New constant: "
    $cvin.onkeyup = event => {
        if (event.key === "Enter" && doAct.onclick === $c) {
            doAct.onclick()
        }
    }
    main.replaceChildren($cvl)
    doAct.onclick = $c
}
export function $c() {
    cDecl(...$cvin.value.split(" "))
}


// $v
export function $vsetup() {
    $cvt.textContent = "New variable: "
    $cvin.onkeyup = event => {
        if (event.key === "Enter" && doAct.onclick === $v) {
            doAct.onclick()
        }
    }
    main.replaceChildren($cvl)
    doAct.onclick = $v
}
export function $v() {
    vDecl(...$cvin.value.split(" "))
}
