const store = {}

// terrible, utterly terrible
const fakeRedis = {type:"Redis",vars:[{
  read: (key) => taskOf(store[key] || ""),
  write: (key) => (value) => {
    store[key] = value;
    return taskOf(unit())
  }
}]}

const unit = () => ({type: "Unit",vars:[]})

const taskOf = (a) => ({ type: "Task",vars: [(callback) => callback(a)]})

const server = (url) => (cb) => {
  // run the function, passing the url
  const t = main(fakeRedis)(url)
  // pass the callback to the generated Task to make it All Happen
  t.vars[0](cb)
}

module.exports = { main: server };
