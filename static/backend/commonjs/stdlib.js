const __app = (tyCon, val) => ({ ...tyCon, vars: [...tyCon.vars, val] });

const __match = (val, matches, catchAll) => {
  const branch = matches[val.type];
  if (!branch) {
    return catchAll;
  }
  if (val.vars.length == 0) {
    return branch;
  }
  return val.vars.reduce((f, a) => f(a), branch);
};

const __eq = (a,b) => JSON.stringify(a) === JSON.stringify(b)

module.exports = {
  __app: __app,
  __match: __match,
  __eq: __eq
}
