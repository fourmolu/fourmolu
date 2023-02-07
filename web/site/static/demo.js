/***** Initialize web worker *****/

const _worker = new Worker('/static/worker.js')

// All worker communication should use `callWorker`, which ensures that
// that only one thread is sending input + waiting for output at a time
let _controller = new Promise((resolve) => {
  // wait for initial message indicating worker is ready
  _worker.onmessage = () => resolve()
})
function callWorker(message) {
  const promise = _controller.then(() =>
    new Promise((resolve) => {
      _worker.postMessage(message)
      _worker.onmessage = (event) => resolve(event.data)
    })
  )
  _controller = promise
  return promise
}

async function runFourmolu(inputText, printerOpts = {}, options = {}) {
  const input = {
    inputText,
    printerOpts,
    ...options,
  }
  const inputRaw = JSON.stringify(input)
  const outputRaw = await callWorker(inputRaw)
  return JSON.parse(outputRaw)
}

/***** Run demo *****/

// Tracks the delay for showing format errors
let errorTimer = null
const errorDelaySecs = 0.5

function getDemoElements() {
  return {
    input: document.querySelector('#demo-app-input'),
    printerOpts: document.querySelectorAll('.demo-printerOpt'),
    options: document.querySelectorAll('.demo-config-option'),
    output: document.querySelector('#demo-app-output'),
    ast: {
      input: document.querySelector('#demo-app-input-ast'),
      output: document.querySelector('#demo-app-output-ast'),
    },
  }
}

function getOptionMap(nodes) {
  return Object.fromEntries(
    Array.from(nodes).map((el) => {
      const name = el.getAttribute('name')
      const value = getOptionValue(el)
      return [name, value]
    })
  )
}
function getOptionValue(el) {
  switch (el.type) {
    case 'number':
      return Number(el.value)
    case 'checkbox':
      return el.checked
    default:
      return el.value === 'null' ? null : el.value
  }
}

async function runDemo() {
  const demo = getDemoElements()
  const input = demo.input.value
  const printerOpts = getOptionMap(demo.printerOpts)
  const options = getOptionMap(demo.options)

  const {
    formatError,
    outputHTML,
    inputAST,
    outputAST,
  } = await runFourmolu(input, printerOpts, options)

  // regardless of the result, clear the old timer
  clearTimeout(errorTimer)

  if (formatError) {
    // show the error after a delay
    errorTimer = setTimeout(
      () => {
        demo.output.classList.add('error')
        demo.output.innerText = formatError
      },
      errorDelaySecs * 1000
    )
  } else {
    // always remove the `error` class, if it was set
    demo.output.classList.remove('error')
    demo.output.innerHTML = outputHTML
  }

  demo.ast.input.innerText = inputAST
  demo.ast.output.innerText = outputAST
}

function main() {
  const demo = getDemoElements()

  demo.input.addEventListener('input', runDemo)
  demo.printerOpts.forEach((el) => el.addEventListener('input', runDemo))
  demo.options.forEach((el) => el.addEventListener('input', runDemo))
}

document.addEventListener('DOMContentLoaded', main)
