import { WASI, OpenFile, File, ConsoleStdout } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/index.js";
import ghc_wasm_jsffi from "/static/ghc_wasm_jsffi.js";

const args = [];
const env = ["GHCRTS=-H64m"];
const fds = [
  new OpenFile(new File([])), // stdin
  ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ''${msg}`)),
  ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ''${msg}`)),
];
const options = { debug: false };
const wasi = new WASI(args, env, fds, options);

const instance_exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch("/static/app.wasm"), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
});
Object.assign(instance_exports, instance.exports);

wasi.initialize(instance);
await instance.exports.hs_start(globalThis.example);

// Basecoat re-initialization system
// Ensures Basecoat components work after Miso's Virtual DOM re-renders
let basecoatInitTimer = null;

function initBasecoatComponents() {
  if (window.basecoat && typeof window.basecoat.initAll === 'function') {
    console.log('[Basecoat] Initializing components');
    window.basecoat.initAll();
  }
}

// Debounced init (Miso may render multiple times quickly)
function scheduleBasecoatInit() {
  if (basecoatInitTimer) clearTimeout(basecoatInitTimer);
  basecoatInitTimer = setTimeout(() => {
    initBasecoatComponents();
    basecoatInitTimer = null;
  }, 100);
}

// MutationObserver detects Miso DOM updates
const observer = new MutationObserver((mutations) => {
  for (const mutation of mutations) {
    if (mutation.type === 'childList' || mutation.type === 'attributes') {
      const target = mutation.target;
      if (target.nodeType === Node.ELEMENT_NODE &&
          (target.hasAttribute('data-popover') ||
           target.hasAttribute('data-tooltip') ||
           target.classList.contains('dropdown-menu') ||
           target.classList.contains('tabs'))) {
        scheduleBasecoatInit();
        return;
      }
    }
  }
});

// Initial init + start observing
scheduleBasecoatInit();
observer.observe(document.body, {
  childList: true,
  subtree: true,
  attributes: true,
  attributeFilter: ['data-popover', 'data-tooltip', 'aria-expanded', 'class']
});
