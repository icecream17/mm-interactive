
/** @type {<A, B extends Callback>(defaul: A, method: B) => ((a: Parameters<A[B]>[0], b=defaul) => ReturnType<A[B]>);} */
const fac2 = (defaul, method) => (a, b=defaul) => b[method](a)
/** Append child to (default: document.body)
 * @type {typeof fac2<HTMLBodyElement, "appendChild">}
 */
const aC2 = fac2(document.body, "appendChild")

/** @type {typeof document.createElement} */
const makeEl = document.createElement.bind(document)

/** @type {<A>(el: A, callback?: (...args: any) => any) => A} */
const elC = (el, callback) => (callback?.(el), el)
/** @type {<A>(el: A, ...children: Node[]) => A} */
const elCh = (el, ...children) => (children.forEach(child => aC2(child, el)), el)

/**
 * callback
 * @type {<K extends keyof HTMLElementTagNameMap>(tag: K, callback: (val: HTMLElementTagNameMap[K]) => any) => HTMLElementTagNameMap[K]} */
const makeElC = (tag, callback) => elC(makeEl(tag), callback)
/**
 * children
 * @type {<K extends keyof HTMLElementTagNameMap>(tag: K, ....children: Node[]) => HTMLElementTagNameMap[K]} */
const makeElCh = (tag, ...children) => elCh(makeEl(tag), ...children)
/**
 * callback + children
 * @param {Parameters<typeof makeElC>[0]} tag
 * @param {Parameters<typeof makeElC>[1]} callback
 * @param {Parameters<typeof elCh> extends [any, ...infer B] ? B : []} children
 */
const makeElCCh = (tag, callback, ...children) => elCh(makeElC(a, b), c)

const concat = (...args) => args.join("")
const propFun = (prop, value) => obj => obj[prop] = value
const propFuns = map => obj => Object.assign(obj, map)

const makeButton = text =>
   makeElC("button", propFun("textContext", text))


// kənsəsəˈrəʊlɪ //
// aka user console
export const [INFO, ERROR, assert, contextualize] = (() => {
   const context = []
   const ucsoleTextarea = makeElC("textarea", propFun("id", "js-ucsolet"))
   aC2(makeElCCh("label", propFun("textContent", "Log"), ucsoleTextarea))

   // Terrible contrast; a11y
   return [
      msg => {
         ucsoleTextarea.style.borderColor = "dodgerblue"
         ucsoleTextarea.value += "\n\n" + msg
      },
      (msg, ...details) => {
         console.error(...details)
         ucsoleTextarea.style.borderColor = "red"
         ucsoleTextarea.value += `\n\nERROR\n${context.join('\n')}\n` + msg
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
      fn => (...args) => {
         context.push(fn.name)
         const result = fn(...args)
         context.pop()
         return result
      }
   ]
})()
