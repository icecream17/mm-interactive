import { apCh1, apCh2, make } from "./html.js";
import { startScope, endScope, cDecl, vDecl, Dvar, Fhyp, Ehyp, Alabe, Plabe } from "./hmm.js";

/** @type {HTMLSelectElement} */
const typdropdown = make("select",
    make("option", "${"),
    make("option", "$}"),
    make("option", "$c"),
    make("option", "$v"),
    make("option", "$f"),
    make("option", "$e"),
    make("option", "$d"),
    make("option", "$a"),
    make("option", "$p"),
    // make("option", "$("),
    // make("option", "$)"),
    // make("option", "$["),
    // make("option", "$]"),
)

apCh2(make("label", "type ", typdropdown))

const main = make("div")
/** @type {HTMLButtonElement} */
const doAct = make("button", "start scope")
doAct.type = "button"

const fns = {
    "${": ["start scope", startScope],
    "$}": ["end scope", endScope],
}

typdropdown.onchange = () => {
    doAct.name = fns[typdropdown.selectedOptions[0].text][0]
    doAct.onclick = fns[typdropdown.selectedOptions[0].text][1]
}
typdropdown.onchange()

apCh1(document.body, main, doAct)
