import { WASI } from '@bjorn3/browser_wasi_shim'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

async function main() {
  const [wasm, hackageInfoBuf] = await Promise.all([
    initWebAssembly(fetch('/static/fourmolu-wasm.wasm')),
    fetch('/static/hackage-info.bin').then((r) => r.arrayBuffer()),
  ])
  const hs = wasm.instance.exports

  const withBytesPtr = (bytes, callback) => {
    const len = bytes.byteLength
    const ptr = hs.malloc(len)
    try {
      new Uint8Array(hs.memory.buffer, ptr, len).set(bytes)
      callback(ptr, len)
    } finally {
      hs.free(ptr)
    }
  }

  hs._initialize()
  hs.hs_init(0, 0)

  withBytesPtr(hackageInfoBuf, hs.initFixityDB)

  self.onmessage = (event) => {
    const inputBytes = encoder.encode(event.data)
    withBytesPtr(inputBytes, (inputPtr, inputLen) => {
      const resultPtr = hs.runFourmolu(inputPtr, inputLen)
      try {
        const outputPtr = hs.getString(resultPtr)
        const outputLen = hs.getStringLen(resultPtr)
        const outputBytes = new Uint8Array(hs.memory.buffer, outputPtr, outputLen)
        const output = decoder.decode(outputBytes)
        self.postMessage(output)
      } finally {
        hs.freeStringWithLen(resultPtr)
      }
    })
  }

  self.postMessage(null)
}

async function initWebAssembly(source) {
  const wasi = new WASI([], [], [])
  const wasm = await WebAssembly.instantiateStreaming(source, {
    wasi_snapshot_preview1: wasi.wasiImport,
  })
  wasi.inst = wasm.instance
  return wasm
}

main()
