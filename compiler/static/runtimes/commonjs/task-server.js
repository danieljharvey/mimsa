const server = (url) => (cb) => {
  // run the function, passing the url
  const t = main(url)
  // pass the callback to the generated Task to make it All Happen
  t.vars[0](cb)
}

module.exports = { main: server };
