/***** Initialize web worker *****/

const _worker = new Worker('/static/worker.js')

// All worker communication should use `callWorker`, which ensures that
// that only one thread is sending input + waiting for output at a time
let _controller = new Promise((resolve) => {
  // wait for initial message indicating worker is ready
  _worker.onmessage = () => {
    // format what's already in the text box
    runDemo()

    // start allowing format requests
    resolve()
  }
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
    warnings: document.querySelector('#demo-warnings'),
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
      // very basic type coercion; let the fourmolu backend do any value checks
      const parsers = el.dataset.parsers.split('|')
      for (const parser of parsers) {
        switch (parser) {
          case 'string':
            return el.value
          case 'number': {
            const val = Number(el.value)
            if (!isNaN(val)) {
              return val
            }
          }
          case 'null': {
            if (el.value === 'null') {
              return null
            }
          }
        }
      }
      throw new Error(`Could not parse value: ${el.value}`)
  }
}

function createElement(tag, children = [], options = {}) {
  const elem = document.createElement(tag)

  elem.append(...children)

  if (options.classes) {
    elem.classList.add(...options.classes)
  }

  return elem
}

function refreshWarnings(demo, printerOpts, options) {
  const warnings = []
  if (printerOpts['column-limit'] != 'none' && options.checkIdempotence) {
    warnings.push('Setting column limit may break idempotence!')
  }

  const nodes =
    warnings.length === 0
      ? []
      : [
        createElement(
          'ul',
          warnings.map(warning => createElement('li', [warning])),
          { classes: ['alert', 'alert-warning'] },
        )
      ]
  demo.warnings.replaceChildren(...nodes)
}

async function runDemo() {
  const demo = getDemoElements()
  const input = demo.input.value
  const printerOpts = getOptionMap(demo.printerOpts)
  const options = getOptionMap(demo.options)

  refreshWarnings(demo, printerOpts, options)

  const outputTextboxOriginal = demo.output.querySelector('pre')
  const [scrollTopOriginal, scrollLeftOriginal] =
    outputTextboxOriginal
      ? [outputTextboxOriginal.scrollTop, outputTextboxOriginal.scrollLeft]
      : [0, 0]

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

    // reset scrollbars
    const outputTextboxNew = demo.output.querySelector('pre')
    outputTextboxNew.scrollTop = scrollTopOriginal
    outputTextboxNew.scrollLeft = scrollLeftOriginal
  }

  demo.ast.input.innerText = inputAST
  demo.ast.output.innerText = outputAST
}

function main() {
  const demo = getDemoElements()

  demo.input.addEventListener('input', runDemo)
  demo.printerOpts.forEach((el) => el.addEventListener('input', runDemo))
  demo.options.forEach((el) => el.addEventListener('input', runDemo))

  // initialize the demo with some code
  demo.input.value = `
    {-# LANGUAGE UnicodeSyntax #-}
    -- | This is the
    -- person module.
    module Person (Person(Person,
    name),
    loadPerson)where
    import Control.Monad
        (void,unless,when)
    data Person = Person
      {name::String,
       age::Int}
    loadPerson ::
        (MonadIO m)
        => Int -> m ()
    loadPerson id = runQuery queryText [SqlInt id]
      >>= toPerson
      where queryText = "SELECT * FROM person WHERE id = ?"
            toPerson rows =
                case rows of
                  [ [ SqlString name, SqlInt age ] ] -> pure Person { .. }
                  _-> let s = "Invalid result: "++show rows in logAndFail s
  `.replace(/^[ ]{4}/gm, '').trim()
  demo.input.setSelectionRange(0, 0) // move cursor to beginning
}

document.addEventListener('DOMContentLoaded', main)
