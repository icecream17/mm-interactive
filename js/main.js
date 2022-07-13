import { apCh1, apCh2, INFO, make } from "./html.js";
import { startScope, endScope, Dvar, Fhyp, Ehyp, Alabe, Plabe } from "./hmm.js";
import { main, doAct, $c, $csetup, $v, $vsetup } from "./mainfns.js"

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

document.body.addEventListener("keydown", ev => {
    if (ev.altKey && ev.key === "/") {
        typdropdown.focus()
    } else if (ev.altKey && ev.key === "?") {
        INFO(`
            Keyboard shortcuts
            Alt + /             Focus dropdown
            Alt + Shift + /     Display this message
        `)
    }
})

// End fn declarations
const noop = () => {}
const fns = {
    "${": ["start scope", startScope, noop],
    "$}": ["end scope", endScope, noop],
    "$c": ["add constant(s)", $c, $csetup],
    "$v": ["add variable(s)", $v, $vsetup],
}

typdropdown.onchange = () => {
    const [name, onclick, setup] = fns[typdropdown.selectedOptions[0].text]
    setup()
    doAct.name = name
    doAct.textContent = name
    doAct.onclick = onclick
}
typdropdown.onchange()

apCh1(document.body, main, doAct)
