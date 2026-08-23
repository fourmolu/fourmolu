# Fourmolu

> Current release: **v$version$**

Fourmolu is a formatter for Haskell source code. It is a fork of [Ormolu](https://github.com/tweag/ormolu), with upstream improvements continually merged. Fourmolu can be seen as a less-opinionated version of Ormolu: Ormolu implements “one true formatting style”, whereas Fourmolu is configurable. Try it out below!

<link rel="stylesheet" href="/static/demo.css" />
<script src="/static/demo.js"></script>
<div id="demo">
    <div id="demo-buttons">
        <button type="button" class="btn btn-primary" data-bs-toggle="modal" data-bs-target="#demo-ast-modal">
            Show internal parse result
        </button>
        <button id="demo-copy-config" type="button" class="btn btn-primary">
            Copy configuration
        </button>
    </div>
    <aside id="demo-warnings"></aside>
    <div id="demo-app">
        <textarea id="demo-app-input" autocomplete="off" autofocus></textarea>
        <div id="demo-app-output"></div>
    </div>
    <div id="demo-options">
        <section>
            <h2>Formatting options</h2>
            <div class="widgets">
                $for(demoOptions)$ $widget$ $endfor$
            </div>
        </section>
        <section>
            <h2>Program options</h2>
            <div class="widgets">
                <label>
                    Check idempotence
                    <input class="demo-config-option" name="checkIdempotence" type="checkbox" checked />
                </label>
                <label>
                    Unsafe mode
                    <input class="demo-config-option" name="unsafeMode" type="checkbox" />
                </label>
                <label>
                    Format as backpack signature
                    <input class="demo-config-option" name="formatBackpack" type="checkbox" />
                </label>
            </div>
        </section>
    </div>
    <div id="demo-ast-modal" class="modal fade" tabindex="-1" aria-hidden="true">
        <div class="modal-dialog modal-xl">
            <div class="modal-content">
                <div class="modal-header">
                    <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
                </div>
                <div id="demo-ast" class="modal-body">
                    <h2>Input</h2>
                    <h2>Output</h2>
                    <pre id="demo-app-input-ast">&nbsp;</pre>
                    <pre id="demo-app-output-ast">&nbsp;</pre>
                </div>
            </div>
        </div>
    </div>
    <div class="toast-container position-fixed top-0 end-0 p-3">
        <div id="demo-toast-copy-config" class="toast text-bg-secondary" role="alert" aria-live="assertive" aria-atomic="true">
            <div class="d-flex">
                <div class="toast-body">
                    Configuration successfully copied!
                </div>
                <button type="button" class="btn-close btn-close-white me-2 m-auto" data-bs-dismiss="toast" aria-label="Close"></button>
            </div>
        </div>
    </div>
</div>
