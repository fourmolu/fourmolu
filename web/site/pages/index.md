# Fourmolu

> Current release: **v$version$**

Fourmolu is a formatter for Haskell source code. It is a fork of [Ormolu](https://github.com/tweag/ormolu), with the intention to continue to merge upstream improvements. Try it out below!

<link rel="stylesheet" href="/static/demo.css" />
<script src="/static/demo.js"></script>
<div id="demo">
    <button id="demo-ast-modal-toggle" type="button" class="btn btn-primary" data-bs-toggle="modal" data-bs-target="#demo-ast-modal">
        Show internal parse result
    </button>
    <div id="demo-app">
        <textarea id="demo-app-input" autocomplete="off" autofocus></textarea>
        <div id="demo-app-output"></div>
    </div>
    <div id="demo-options">
        $for(demoOptions)$ $widget$ $endfor$
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
</div>
