import { INFO, assert, contextualize } from "./html.js";
import { copySet, eqArr, setDiff, setUnion } from "./utils.js";

// TODO: comments

/** @type {Scope} */
let currscope;
class Scope {
   /** @param {Scope} [parent] */
   constructor (parent) {
      this.parent = parent
      currscope = this

      /** @type {Map<string, Tok>} */
      this.tokRegistry = new Map()
      /** @type {Set<Ehyp>} */
      this._ehyps = new Set()
      /** @type {Set<Dvar>} */
      this._dvars = new Set()
   }

   /** @return {boolean} */
   hasTok(tokstr) {
      return this.tokRegistry.has(tokstr) || this.parent?.hasTok(tokstr)
   }

   /** @return {Tpk} */
   getTok(tokstr) {
      const r = this.tokRegistry.get(tokstr) ?? this.parent?.getTok(tokstr)
      assert(r !== undefined, `getTok: ${tokstr} does not exist!`)
      return r
   }

   addTok(tok) {
      this.tokRegistry.set(tok.name, tok)
   }

   isConst(c) {
      return this.getTok(c).type === "c"
   }

   isVar(v) {
      return this.getTok(v).type === "v"
   }

   /** @return {boolean} */
   hasEhyp(e) {
      return this.ehyps.has(e) || this.parent?.hasEhyp(e)
   }

   /**
    * Whether two variables are distinct
    * @return {boolean}
    */
   dVar(a, b) {
      for (const dvar of this.dvars) {
         if (dvar.has(a) && dvar.has(b)) {
            return true
         }
      }
      return this.parent?.dVar(a, b) ?? false
   }

   /** @return {typeof this._ehyps} */
   get ehyps () {
      if (this.parent == null) {
         return copySet(this._ehyps)
      } else {
         return setUnion(this.parent.ehyps, this._ehyps)
      }
   }

   /** @return {typeof this._dvars} */
   get dvars () {
      if (this.parent == null) {
         return copySet(this._dvars)
      } else {
         return setUnion(this.parent.dvars, this._dvars)
      }
   }

   get depth () {
      if (this.parent == null) {
         return 0
      } else {
         return this.parent.depth + 1
      }
   }
}

const glscope = new Scope()
/** @type {Map<Var, Fhyp>} */
const fhyps = new Map() // Floating hypotheses are global
/** @type {Map<string, Label>} */
const labels = new Map() // Hypotheses, assertions, and theorems
/** @type {Set<string>} */
const wasvar = new Set() // Only to prevent constants and previously defined variables to have conflicting names
const declarationOrder = []
const validSymbol = tok => /^[a-zA-Z0-9!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~]+$/.test(tok)
const validLabel = tok => /^[a-zA-Z0-9\-_.]+$/.test(tok) // Unrequired "\" escape for clarity

// Metamath language - corresponds to "${"
export function startScope () {
   const s = new Scope(currscope)
   INFO(`depth ${s.depth}`)
   currscope = s
   return s
}

// Metamath language - corresponds to "$}"
export function endScope () {
   assert(currscope !== glscope, "Cannot end global scope")
   INFO(`depth ${currscope.parent.depth}`)
   return currscope = currscope.parent
}

// Try to do all assertions before setting any values

/**
 * @param {"c" | "v"} type
 * @param {string} name
 * @typedef {Tok & {type: "c"}} Const
 * @typedef {Tok & {type: "v"}} Var
 */
function Tok (type, name) {
   assert(type === "c" || type === "v", `Type ${type} must be "c" or "v"`)
   assert(!currscope.hasTok(name), `${name} already exists!`)
   assert(validSymbol(name), `The token ${name} is invalid!`)
   if (type === "c") {
      assert(glscope === currscope, `Constants (here ${name}) must be declared in global scope`)
      assert(!wasvar.has(name), `${name} cannot be a constant because it was previously defined as a variable!`)
   } else {
      wasvar.add(name)
   }
   this.type = type
   this.name = name
   currscope.addTok(this)
   declarationOrder.push(this)
}

function tokeq(toka, toke) {
   if (toka.type === "c") {
      // Constants are global (and only declared once)
      return toka === toke
   } else {
      // Variables are equivalent if their fhyp typecodes are equal
      return toke.type === "v" && fhyps.get(toka) === fhyps.get(toke)
   }
}

// Metamath language - constant and variable declarations
// The "name" of a token is its math-symbol.
//   $c math-symbol ...math-symbol[] $.
//   $v math-symbol ...math-symbol[] $.
export const cDecl = (...names) => names.map(name => new Tok("c", name))
export const vDecl = (...names) => names.map(name => new Tok("v", name))

// Metamath language - Distinct variable condition
//   $d variable variable ...variable[] $.
/**
 * Although the specification doesn't have this check,
 * no variables can be the same in a distinct variable condition.
 * @param {string[]} vars - Actually a set of variable names
 * @typedef {Set<string>} Dvar
 */
export function Dvar (...vars) {
   /** @type {Dvar} */
   const vs = new Set(vars)
   assert(vs.size === vars.length, `No duplicate variables in a distinct variable condition! ${vars}`)
   assert(vars.every(v => currscope.hasTok(v)), `Undeclared symbols! ${vars.filter(v => !currscope.hasTok(v))}`)
   currscope.dvars.add(vs)
   declarationOrder.push(vs)
}


class Expr {
   constructor (str) {
      /** @type {Tok[]} */
      this.toks = str.trim().split(' ').map(tok => currscope.getTok(tok))
   }

   get vars () {
      return this.toks.filter(tok => tok.type === "v")
   }
}

class Label {
   constructor (type, name, str) {
      assert(validLabel(name), `${name} is an invalid label! (only a-zA-Z0-9 and _.-)`)
      assert(!labels.has(name), `No duplicates! ${type} ${name}`)
      assert(!currscope.hasTok(name), `No duplicates! [Label-Math tok conflict] ${type} ${name}`)
      this.type = type
      this.name = name // Label
      this.assertion = new Expr(str)
      this.vars = this.assertion.vars
      assert(currscope.isConst(this.assertion.toks[0]), `${type} ${name}; Labels must start with a typecode (constant):\n${str}`)
      if (type !== "fhyp") {
         const invalidvars = this.vars.filter(v => !fhyps.has(v))
         assert(invalidvars.length === 0, `${type} ${name}: Before using a variable it must appear in an fhyp\n${invalidvars}`)
      }
   }
}

/** Run when all checks on a label pass */
function labelPass (self) {
   labels.set(self.name, self)
   declarationOrder.push(self)
}

// Metamath language - "floating" hypothesis
//   label $f typecode variable $.
export class Fhyp extends Label {
   constructor (name, str) {
      super("fhyp", name, str)
      assert(this.assertion.toks.length === 2, `fhyp must have two tokens, got:\n${this.assertion.toks}`)
      assert(currscope.isVar(this.assertion.toks[1]), `the second token in an fhyp must have a variable:\n${str}`)
      assert(!fhyps.has(this.assertion.toks[1]), `variable already in an fhyp! duplicates:\n${fhyps.get(this.assertion.toks[1])}\n${name}`)
      fhyps.set(this.assertion.toks[1], this)
      labelPass(this)
   }
}

// Metamath language - essential hypothesis
//   label $e typecode ...math-symbol[] $.
export class Ehyp extends Label {
   constructor (name, str) {
      super("ehyp", name, str)
      currscope.ehyps.add(this)
      labelPass(this)
   }
}

class AssertedLabel extends Label {
   constructor (type, name, str) {
      super(type, name, str)

      // Mandatory frame props
      /** @type {Set<Ehyp>} */
      this.mandatoryHyps = new Set()
      this.mandatoryVars = new Set(this.vars)
      for (const ehyp of currscope.ehyps) {
         this.mandatoryHyps.add(ehyp)
         for (const va of ehyp.vars) {
            this.mandatoryVars.add(va)
         }
      }

      /** @type {Set<Dvar>} */
      this.mandatoryDvars = new Set()
      for (const dvarc of currscope.dvars) {
         for (const dvar of dvarc) {
            if (this.mandatoryVars.has(currscope.getTok(dvar))) {
               this.mandatoryDvars.add(dvarc)
            }
         }
      }

      // Check all vars are covered
      const coveredMandatoryVars = new Set()
      for (const [v, fhyp] of fhyps) {
         if (this.mandatoryVars.has(v)) {
            this.mandatoryHyps.add(fhyp)
            coveredMandatoryVars.add(v)
         }
      }

      assert(this.mandatoryVars.size === coveredMandatoryVars.size, `Not all variables in ${name} are covered by $f hypotheses: ${[...coveredMandatoryVars]} out of ${[...this.mandatoryVars]}`)

      // Order mandatory hyps
      const mHyps = new Set()
      for (const stmt of declarationOrder) {
         if (this.mandatoryHyps.has(stmt)) {
            mHyps.add(stmt)
         }
      }
      this.mandatoryHyps = mHyps

      // Optional frame hyps
      if (type !== "Axiom") {
         this.optionalVars = new Set()
         this.optionalHyps = new Set()

         // TODO: Optimization - don't recalc fhyp vars
         for (const [v, fhyp] of fhyps) {
            if (!this.mandatoryVars.has(v)) {
               this.optionalVars.add(v)
               this.optionalHyps.add(fhyp)
            }
         }

         // Distinct variable condition
         this.optionalDvars = setDiff(currscope.dvars, this.mandatoryDvars)

         /** @type {Map<str, Dvar>} */
         this.associatedDvarPairs = new Map()
         for (const dvar of currscope.dvars) {
            for (const tok1 of dvar) {
               this.associatedDvarPairs.set(tok1, dvar)
            }
         }
      }
   }
}

// Metamath language - Axiomatic assertions
export class Alabe extends AssertedLabel {
   constructor (name, str) {
      super("Axiom", name, str)
      labelPass(this)
   }
}

// Metamath language - Provable assertions
export class Plabe extends AssertedLabel {
   /**
    * @param {string[]} proof
    */
   constructor (name, str, proof) {
      super("Theorem", name, str)
      this.proof = proof
      verifyProof(this)
      labelPass(this)
   }
}

/**
 * @param {Plabe} plabe
 */
function verifyProof(plabe) {
   // Definitions:
   //
   // Substitution map: Maps variables to expressions
   // Substitution: Replacement of variables with what they map to

   /** @type {Tok[][]} */
   const stack = []

   if (plabe.proof.includes("?")) {
      // A proof may contain a ? indicating an unknown step.
      // A proof verifier may ignore any proof containing ?
      // but should warn the user that the proof is incomplete.
      INFO(`incomplete proof: ${plabe.name}`)

      // hmm
      // 4.4.6 For the purposes of parsing a proof, the ? will push a single
      // entry onto the stack just as if it were a hypothesis.
      return
   }

   const labelrefs = plabe.proof.map(label => {
      assert(labels.has(label), `Label ${label} in the proof of ${plabe.name} does not exist!`)
      /** @type {Label} */
      const ref = labels.get(label)
      if (ref.type === "ehyp") {
         assert(currscope.hasEhyp(ref), `Ehyp ${ref.name} in the proof of ${plabe.name} is not in scope!`)
      }
      return ref
   })

   // A proof is a sequence of labels
   for (const [index, ref] of labelrefs.entries()) {
      // If the label refers to an active hypothesis, the expression in the
      // hypothesis is pushed onto the stack
      if (ref.type === "fhyp" || ref.type === "ehyp") {
         stack.push(ref.assertion.toks)
      }
      // If the label refers to an assertion, a (unique) substitution must
      // exist that, when made to the mandatory hypotheses of the referenced
      // assertion, causes them to match the topmost entries of the stack,
      // in order of occurrence of the mandatory hypotheses, with the topmost
      // stack entry matching the last mandatory hypothesis of the referenced
      // assertion.
      else if (ref.type === "Axiom" || ref.type === "Theorem") {
         // As many stack entries as there are mandatory hypotheses are then
         // popped from the stack.
         const numMandatoryHyps = ref.mandatoryHyps.size
         assert(stack.length >= numMandatoryHyps, `Theorem ${plabe.name}, step[${index}] ${ref.name} requires ${numMandatoryHyps} hypotheses but only ${stack.length} expressions on the proof stack`)

         const topmostEntries = stack.splice(stack.length - numMandatoryHyps, numMandatoryHyps)

         // (Find substitution map)
         const submap = canUnify(topmostEntries, ref.mandatoryHyps)
         assert(submap !== false, `Theorem ${plabe.name}, step[${index}] ${ref.name}'s hypothes(es) cannot be unified with the proof expression stack:\nHyps:\n${ref.mandatoryHyps}\n\nStack:\n${topmostEntries}`)

         // (Check distinct variable conditions)
         contextualize(dvCheck, `Theorem ${plabe.name}, step[${index}]`)(submap, ref, plabe)

         // The same substitution is then made to the referenced assertion,
         // and the result is pushed onto the stack.
         stack.push(substitute(submap, ref.assertion))
      }
   }

   // After the last label in the proof is processed, the stack must have a single
   // entry that matches the expression in the $p statement containing the proof
   assert(stack.length === 1, `Theorem ${plabe.name}, step[end]'s stack does not have only one entry`)
   assert(eqArr(stack[0], plabe.assertion), `Theorem ${plabe.name}, step[end] The proved statement and the assertion don't match:\nAssertion:\n${plabe.assertion}\n\nStack:\n${stack[0]}`)
}

/**
 * @param {Map<Tok, Tok[]>} submap - substitution map
 * @param {AssertedLabel} ref
 * @param {Plabe} plabe
 */
function dvCheck (submap, ref, plabe) {
   const mandatoryDvars = ref.mandatoryDvars
   for (const dvar of mandatoryDvars) {
      // Set of subvars (maps to vname for debugging purposes)
      const subvars = new Map()

      // The $d statement is actually more general, as the "disjoint" in the
      // name suggests. The full meaning is that if any substitution is made to
      // two variables [...], the two expressions resulting from the
      // substitution must have no variables in common.
      // In addition, each possible pair of variables, one from each expression,
      // must be in a $d statement associated with the statement being proved.

      // Say u is a variable.
      // If the restriction
      //
      //                       $d A B $.
      //
      // has been specified for a theorem referenced in a proof,
      // we may not substitute A with A + u and B with B + u
      // because these two symbol sequences have the variable u in common.
      //
      // Furthermore, if a and b are variables, we may not substitute A with a
      // and B with b unless we have also specified $d a b for the theorem
      // being proved; in other words, the $d property associated with a pair
      // of variables must be effectively preserved after substitution.
      for (const vname of dvar) {
         const v = currscope.getTok(vname)
         if (submap.has(v)) {
            // [variables in the expression resulting from a substitution...
            for (const subtok of submap.get(v)) {
               if (currscope.isVar(subtok)) {
                  // ...cannot be the same]
                  assert(!subvars.has(subtok), `Invalid substitution! Cannot use the same variable ${subtok.name} in both $d ${vname} ${subvars.get(subtok)}`)
                  subvars.set(subtok, vname)
               }
            }
         }
      }

      // [$d must be preserved]
      for (const comb1 of subvars.keys()) {
         for (const comb2 of subvars.keys()) {
            if (comb1 !== comb2) {
               assert(plabe.associatedDvarPairs.get(comb1.name).has(comb2.name), `Invalid substitution! $d not preserved in $d ${subvars.get(comb1)} ${subvars.get(comb2)} because there is no $d ${comb1.name} ${comb2.name}`)
            }
         }
      }
   }
}

/**
 * Returns false or the required substitutions to make
 * @param {Tok[][]} esa
 * @param {Set<Ehyp>} hyps
 */
function canUnify(esa, hyps) {
   if (esa.length !== hyps.size) {
      return false
   }

   /** @type {[Tok[], Tok[]][]} */
   const pairs = [...hyps].map(((hyp, i) => [esa[i], hyp.assertion.toks]))

   // In order to solve unification for two groups of expressions,
   // let's first solve unification for two groups of one expression.

   // So consider
   // ( ph1 -> ps1 )
   // ( ph2 -> ps2 )

   // Is there a unification other than the obvious identity?
   // There is!
   // ph1 becomes "ph -> ps", ps1 becomes "ps"
   // ph2 becomes "ph", ps2 become "ps -> ps"
   // ( ph -> ps -> ps )
   // ( ph -> ps -> ps )

   // Really for any two expressions there's an infinite set of
   // unifications. Even the simplest possible one to one variable.
   // In the case of that last one, though, all substitutions are instances
   // of identity.

   // But the metamath pdf says that there's must be one substitution.
   // This means that the grammar must be unambiguous.

   // Now here's another example
   // ( ph1 -> ps1 )
   // ( A1 e. B1 -> ph2 )

   // In order to figure this one out, there needs to be a way to show that
   // A1 "ph1 ->"
   // for example, isn't possible.

   // And this is how typecodes are used by the compiler:
   // to see what particular symbols could be in a variable.

   // Luckily, because of how fhyps work, only a few rules need to be implemented:
   //  Skip any shared starting constants
   //  A lone variable is substituted for the other expression

   // Specifically, any variable must be in an fhyp before it is used
   // in an ehyp or assertion.

   // And the mandatory hypotheses of an assertion label
   // include all fhyps whose variables are in mandatory ehyps.

   // So consider ax-mp:
   //  mandatory hyps
   //   ax-mp.major $e |- ph
   //   ax-mp.minor $e |- ( ph -> ps )
   //   wph $f wff ph
   //   wps $f wff ps

   // And the following stack:
   //   |- ph     |- ( ph -> ph )      wff ph      wff ph

   // Looking at the first one, the starting constant (typecode) "|-"
   // will be removed, and then the mvar "ph" is mapped to the stck "ph"
   // mhyp |- ph
   // stac |- ph

   // For the second one, the current strategy can't figure out either.

   // So how will `ps` be figured out?
   // This is where the fhyps come in: the fourth mhyp and stack entry match
   // exactly the ps variable!
   // mhyp wff ps
   // stac wff ph

   // For any variable used in an ehyp, its fhyp is an mhyp.
   // And thus, all variables have easy to find mappings.

   /** @type {Map<Tok, Tok[]>} */
   const hypsubsts = new Map()
   for (const [exa, hyp] of pairs) {
      for (let i = 0; i < exa.length && i < hyp.length; i++) {
         const eq = tokeq(exa[i], hyp[i])
         const isv = hyp[i].type === "v"
         const isev = exa[i].type === "v"

         if (eq && !isv) {
            continue // Shared constants
         } else if (!isv && !isev) {
            return false // Different constants
         } else if (isv && i === hyp.length - 1) {
            hypsubsts.set(hyp[i], exa.slice(i)) // Lone variable after starting constants
         } else {
            break // Not starting constants and not lone variable
         }
      }
   }

   // Check if substitutions worked
   const subpairs = _subpairs(hypsubsts, pairs)
   assert(subpairs.every(pair => pair[0] === pair[1]), `Unification didn't work?`, exasubsts, hypsubsts, pairs, subpairs)

   //
   return hypsubsts
}

/** @type {<T>(hsubsts: Map<Tok, Tok[]>, pairs: [T, Tok[]][]) => [T, Tok[]][]} */
function _subpairs (hsubsts, pairs) {
   return pairs.map(([exa, hyp]) => [exa, substitute(hsubsts, hyp)])
}

/**
 * @param {Map<Tok, Tok[]>} substs - substitution map
 * @param {Tok[]} expr
 * @return {Tok[]}
 */
function substitute (substs, expr) {
   // if substs.has(tok), replace with ...substs.get(tok)
   return expr.flatMap(tok => substs.has(tok) ? substs.get(tok) : tok)
}
