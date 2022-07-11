export const eqArr = (a, b) => a.every((val, i) => val === b[i])
export const copySet = set => new Set([...set])
export const setDiff = (a, b) => new Set([...a].filter(e => !b.has(e)))
export const setUnion = (a, b) => new Set([...a, ...b])
