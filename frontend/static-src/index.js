import { WASI, OpenFile, File, ConsoleStdout } from "../../static/wasi/index.js";

// Build ghc_wasm_jsffi URL with cache-busting hash if available.
// COMPETENCES_BASE is the mount-point prefix (e.g. "/9c") injected by the
// backend; empty for subdomain mode so URLs stay absolute-from-root.
const basePath = window.COMPETENCES_BASE || '';
const jsffiHash = window.COMPETENCES_JSFFI_HASH || '';
const jsffiUrl = jsffiHash ? `${basePath}/static/ghc_wasm_jsffi.js?v=${jsffiHash}` : `${basePath}/static/ghc_wasm_jsffi.js`;
console.log(`Loading JSFFI from: ${jsffiUrl}`);

// Dynamic import for cache-busting support
const { default: ghc_wasm_jsffi } = await import(jsffiUrl);

const args = [];
const env = ["GHCRTS=-H64m"];
const fds = [
  new OpenFile(new File([])), // stdin
  ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ''${msg}`)),
  ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ''${msg}`)),
];
const options = { debug: false };
const wasi = new WASI(args, env, fds, options);

// Build WASM URL with cache-busting hash if available
const wasmHash = window.COMPETENCES_WASM_HASH || '';
const wasmUrl = wasmHash ? `${basePath}/static/app.wasm?v=${wasmHash}` : `${basePath}/static/app.wasm`;
console.log(`Loading WASM from: ${wasmUrl}`);

const instance_exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch(wasmUrl), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
});
Object.assign(instance_exports, instance.exports);

wasi.initialize(instance);
document.querySelector('body > div')?.remove();
await instance.exports.hs_start(globalThis.example);
