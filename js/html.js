
// Children include strings coerced to text nodes, so useful

/** @param {Node} parent */
export const apCh2 = (child, parent = document.body) => (parent.appendChild(child), parent)
/** @param {Element} parent */
export const apCh1 = (parent, ...children) => (parent.append(...children), parent)

/** @type {typeof document.createElement} */
export const makeEl = document.createElement.bind(document)

/** @type {<A>(el: A, callback?: (...args: any) => any) => A} */
const elC = (el, callback) => (callback?.(el), el)

/**
 * callback
 * @type {<K extends keyof HTMLElementTagNameMap>(tag: K, callback: (val: HTMLElementTagNameMap[K]) => any) => HTMLElementTagNameMap[K]} */
export const makeElC = (tag, callback) => elC(makeEl(tag), callback)
/**
 * children
 * @type {<K extends keyof HTMLElementTagNameMap>(tag: K, ...children: Node[]) => HTMLElementTagNameMap[K]} */
export const makeElCh = (tag, ...children) => apCh1(makeEl(tag), ...children)
/**
 * callback + children
 * @param {Parameters<typeof makeElC>[0]} tag
 * @param {Parameters<typeof makeElC>[1]} callback
 * @param {Parameters<typeof elCh> extends [any, ...infer B] ? B : []} children
 */
export const makeElCCh = (tag, callback, ...children) => elCh(makeElC(tag, callback), ...children)

export const propFun = (prop, value) => obj => obj[prop] = value
export const propFuns = map => obj => Object.assign(obj, map)

export const make = makeElCh

// kənsəsəˈrəʊlɪ //
// aka user console
export const [INFO, ERROR, assert, contextualize] = (() => {
   const context = []
   const ucsoleTextarea = makeElC("textarea", propFuns({
      className: "width100",
      rows: 7,
      onchange() {
         ucsoleTextarea.scrollTop = ucsoleTextarea.scrollHeight
      },
   }))
   apCh2(make("label", "Log\n", ucsoleTextarea))

   const _add = msg => {
      if (ucsoleTextarea.value === "") {
         ucsoleTextarea.value += msg
      } else if (ucsoleTextarea.value.endsWith("\n")) {
         ucsoleTextarea.value += "\n___\n" + msg
      } else {
         ucsoleTextarea.value += "\n\n___\n" + msg
      }
      ucsoleTextarea.scrollTop = ucsoleTextarea.scrollHeight
   }

   // Terrible contrast; a11y
   return [
      msg => {
         ucsoleTextarea.style.borderColor = "dodgerblue"
         _add(msg)
      },
      (msg, ...details) => {
         console.error(...details)
         ucsoleTextarea.style.borderColor = "red"
         _add(`ERROR: ${msg}\n   at ${context.join('\n   at ')}`)
      },
      /** If the condition is falsy, show an error msg to the user and throws an error. */
      /** @type {(condition: unknown, msg: string, ...details: unknown[]) => asserts condition} */
      (condition, msg, ...details) => {
         if (!condition) {
            ERROR(msg, ...details)
            throw new Error(msg) // To prevent code execution
         }
      },
      /** @type <F>(fn: F) => (...args: any) => ReturnType<F> */
      (fn, ctxtxt=fn.name) => (...args) => {
         context.push(ctxtxt)
         const result = fn(...args)
         context.pop()
         return result
      }
   ]
})()
