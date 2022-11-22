let sourceCode = `main {
    let x = 1;

    let Hello = "Hello"; let d = 2;













let heyyyyy = r;
    {}{}
 }`;

import { default as AnsiUp } from "ansi_up";

/* --------------------------------------------------------------------------------------------
 * Copyright (c) 2018-2022 TypeFox GmbH (http://www.typefox.io). All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
// import "monaco-editor/esm/vs/editor/editor.all.js";

import ShadeupEnvironment from "./shadeup.js";

import MonarchDef from "./monarch.js";

// // support all editor features
// import "monaco-editor/esm/vs/editor/standalone/browser/accessibilityHelp/accessibilityHelp.js";
// import "monaco-editor/esm/vs/editor/standalone/browser/inspectTokens/inspectTokens.js";
// import "monaco-editor/esm/vs/editor/standalone/browser/iPadShowKeyboard/iPadShowKeyboard.js";
// import "monaco-editor/esm/vs/editor/standalone/browser/quickAccess/standaloneHelpQuickAccess.js";
// import "monaco-editor/esm/vs/editor/standalone/browser/quickAccess/standaloneGotoLineQuickAccess.js";
// import "monaco-editor/esm/vs/editor/standalone/browser/quickAccess/standaloneGotoSymbolQuickAccess.js";
// import "monaco-editor/esm/vs/editor/standalone/browser/quickAccess/standaloneCommandsQuickAccess.js";
// import "monaco-editor/esm/vs/editor/standalone/browser/quickInput/standaloneQuickInputService.js";
// import "monaco-editor/esm/vs/editor/standalone/browser/referenceSearch/standaloneReferenceSearch.js";
// import "monaco-editor/esm/vs/editor/standalone/browser/toggleHighContrast/toggleHighContrast.js";

//my editor component file
import "monaco-editor/esm/vs/basic-languages/css/css.contribution";
import "monaco-editor/esm/vs/basic-languages/xml/xml.contribution";
import "monaco-editor/esm/vs/basic-languages/javascript/javascript.contribution";

import EditorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker";
import TsWorker from "monaco-editor/esm/vs/language/typescript/ts.worker?worker";
import JsonWorker from "monaco-editor/esm/vs/language/json/json.worker?worker";
import CssWorker from "monaco-editor/esm/vs/language/css/css.worker?worker";
import HtmlWorker from "monaco-editor/esm/vs/language/html/html.worker?worker";

// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore
window.MonacoEnvironment = {
  getWorker(_: string, label: string) {
    if (label === "typescript" || label === "javascript") return new TsWorker();
    if (label === "json") return new JsonWorker();
    if (label === "css") return new CssWorker();
    if (label === "html") return new HtmlWorker();
    return new EditorWorker();
  },
};

import * as monaco from "monaco-editor";

// import {
//   MonacoLanguageClient,
//   CloseAction,
//   ErrorAction,
//   MonacoServices,
//   MessageTransports,
// } from "monaco-languageclient";
// import {
//   BrowserMessageReader,
//   BrowserMessageWriter,
// } from "vscode-languageserver-protocol/browser.js";
// import { StandaloneServices } from "vscode/services";
// import getMessageServiceOverride from "vscode/service-override/messages";

// StandaloneServices.initialize({
//   ...getMessageServiceOverride(document.body),
// });

// buildWorkerDefinition("dist", new URL("", window.location.href).href, false);

// register Monaco languages
monaco.languages.register({
  id: "plaintext",
  extensions: [".txt"],
  aliases: ["PLAINTEXT", "plaintext"],
  mimetypes: ["text/plain"],
});

// create Monaco editor
const editorText = `struct t {
  x: int
}

impl t {
  fn test() {}
}

fn test() -> t {
  let x = t {};
  let y = x.x;
  return x;
}

fn frame() {
  let x = test().test().x;
}`;
let ed = monaco.editor.create(document.getElementById("container")!, {
  model: monaco.editor.createModel(
    editorText,
    "shadeup",
    monaco.Uri.parse("model.shadeup")
  ),
  theme: "vs-dark",
  glyphMargin: true,
  lightbulb: {
    enabled: true,
  },
});

monaco.languages.register({
  id: "shadeup",
  extensions: [".shadeup"],
  aliases: ["SHADEUP", "shadeup"],
  mimetypes: ["text/shadeup"],
});

const editorTextJs = `let x = 12;`;
let edjs = monaco.editor.create(document.getElementById("containerjs")!, {
  value: editorTextJs,
  language: "javascript",
  theme: "vs-dark",
});

// monaco.languages.register

monaco.languages.setMonarchTokensProvider("shadeup", MonarchDef);

// function createLanguageClient(
//   transports: MessageTransports
// ): MonacoLanguageClient {
//   return new MonacoLanguageClient({
//     name: "Sample Language Client",
//     clientOptions: {
//       // use a language id as a document selector
//       documentSelector: [{ language: "shadeup" }],
//       // disable the default error handler
//       errorHandler: {
//         error: () => ({ action: ErrorAction.Continue }),
//         closed: () => ({ action: CloseAction.DoNotRestart }),
//       },
//     },
//     // create a language client connection to the server running in the web worker
//     connectionProvider: {
//       get: () => {
//         return Promise.resolve(transports);
//       },
//     },
//   });
// }

// // install Monaco language client services
// MonacoServices.install();

let env = new ShadeupEnvironment();
env.load();

function updateAlerts() {
  let alerts = [...env.files.keys()].map((k) => env.getAlerts(k)).flat();
  monaco.editor.setModelMarkers(ed.getModel(), "shadeup", [
    ...alerts.map((e) => {
      const ansi_up = new AnsiUp();

      let html = ansi_up.ansi_to_html(e.message);
      return {
        severity: monaco.MarkerSeverity.Error,
        startLineNumber: e.location.start_line_and_column[0],
        startColumn: e.location.start_line_and_column[1] + 1,
        endLineNumber: e.location.end_line_and_column[0],
        endColumn: e.location.end_line_and_column[1] + 1,
        message: e.simple_message,
      };
    }),
  ]);

  let consol = document.getElementById("console");
  if (consol) {
    consol.innerHTML = `${alerts
      .map((e) => {
        const ansi_up = new AnsiUp();

        let html = ansi_up.ansi_to_html(e.message);
        return `<pre>${html}</pre>`;
      })
      .join("")}`;
  }
}

(async () => {
  await env.loaded();

  function spanToRange(span: { start: number; end: number }) {
    let start = ed.getModel()!.getPositionAt(span.start);
    let end = ed.getModel()!.getPositionAt(span.end);
    return new monaco.Range(
      start.lineNumber,
      start.column,
      end.lineNumber,
      end.column
    );
  }

  function offsetToRange(offset: number) {
    let start = ed.getModel()!.getPositionAt(offset);
    let end = ed.getModel()!.getPositionAt(offset);
    return new monaco.Range(
      start.lineNumber,
      start.column,
      end.lineNumber,
      end.column
    );
  }
  function positionToOffset(position: monaco.IPosition) {
    return ed.getModel()!.getOffsetAt(position);
  }

  monaco.languages.registerCompletionItemProvider("shadeup", {
    triggerCharacters: ["."],
    provideCompletionItems(model, position) {
      let suggestions: monaco.languages.CompletionItem[] = [];
      let offset = positionToOffset(position);

      let intel = env.getIntellisense("model.shadeup");

      for (let hint of intel) {
        if (hint.start <= offset && offset <= hint.end) {
          for (let suggestion of hint.suggestions) {
            if (suggestion.label.startsWith("__")) {
              continue;
            }
            let item: monaco.languages.CompletionItem = {
              label: suggestion.label,
              detail: suggestion.detail,
              insertTextRules:
                monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
              kind:
                {
                  Field: monaco.languages.CompletionItemKind.Field,
                  Function: monaco.languages.CompletionItemKind.Function,
                  Variable: monaco.languages.CompletionItemKind.Variable,
                  Type: monaco.languages.CompletionItemKind.Struct,
                }[suggestion.kind as string] ||
                monaco.languages.CompletionItemKind.Function,
              documentation: suggestion.documentation,
              insertText: suggestion.value,
              range: spanToRange({ start: hint.start, end: hint.end }),
            };
            suggestions.push(item);
          }
        }
      }

      let symbols = env.getSymbols();

      if (suggestions.length == 0) {
        suggestions = [
          ...symbols
            .filter((e) => !e.imported || e.aliased)
            .map((e) => ({
              label: e.name,
              detail: e.file,
              insertText: e.name + (e.kind == "function" ? "()" : ""),
              insertTextRules:
                monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
              kind:
                e.kind == "function"
                  ? monaco.languages.CompletionItemKind.Function
                  : monaco.languages.CompletionItemKind.Struct,
              command: {
                id: "editor.action.shadeup.import",
                title: "Import",
                arguments: [e],
              },
            })),
          {
            label: "import",
            insertText: 'import ${1:} from "${2:}";',
            insertTextRules:
              monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
          },
        ];
      }

      return {
        suggestions,
      };
    },
  });

  monaco.editor.registerCommand("editor.action.shadeup.import", (a, e) => {
    let sym = e.name;
    let path = e.file;
    // Parse current imports
    let mod = ed.getModel();
    let finalImport = 0;
    let imports = env.getImports("model.shadeup");

    if (e.imported || e.file == "model.shadeup") {
      return;
    }

    for (let imp of imports) {
      if (imp.path == path) {
        if (imp.imports.find((i) => (i.alias || i.name) == e.name)) {
          return;
        }
        mod.pushEditOperations(
          [],
          [
            {
              range: offsetToRange(
                imp.imports[imp.imports.length - 1].span.start
              ),
              text: sym + ", ",
            },
          ],
          () => null
        );
        return;
      }
      finalImport = imp.span.end;
    }

    let r = spanToRange({
      start: finalImport,
      end: finalImport,
    });

    mod.pushEditOperations(
      [],
      [
        {
          range: {
            startLineNumber: r.startLineNumber - 1,
            startColumn: 1,
            endLineNumber: r.endLineNumber - 1,
            endColumn: 1,
          },
          text: `import ${sym} from "${path}";\n`,
        },
      ],
      () => null
    );
  });

  monaco.languages.registerHoverProvider("shadeup", {
    provideHover(model, pos) {
      let hovers = [];
      let offset = positionToOffset(pos);

      let intel = env.getIntellisense("model.shadeup");

      for (let hint of intel) {
        if (hint.start <= offset && offset <= hint.end) {
          let item = {
            contents: [{ value: "```shadeup\n" + hint.label + "\n```" }],
            range: spanToRange({ start: hint.start, end: hint.end }),
            span: { start: hint.start, end: hint.end },
          };
          hovers.push(item);
        }
      }
      hovers.sort(
        (a, b) => a.span.end - a.span.start - (b.span.end - b.span.start)
      );

      if (hovers.length > 0) {
        return hovers[0];
      }
    },
  });
  monaco.languages.registerDefinitionProvider("shadeup", {
    provideDefinition(model, pos) {
      let gotos = [];
      let offset = positionToOffset(pos);

      let intel = env.getIntellisense("model.shadeup");

      for (let hint of intel) {
        if (hint.start <= offset && offset <= hint.end) {
          let item = {
            range: spanToRange({
              start: hint.goto_offset,
              end: hint.goto_offset,
            }),
            span: { start: hint.start, end: hint.end },
          };
          gotos.push(item);
        }
      }
      gotos.sort(
        (a, b) => a.span.end - a.span.start - (b.span.end - b.span.start)
      );

      if (gotos.length > 0) {
        return {
          uri: monaco.Uri.parse("model.shadeup"),
          range: gotos[0].range,
        };
      }
    },
  });

  env.updateFile("model.shadeup", ed.getValue());
  let successfulParse = env.evaluate("model.shadeup");
  document.querySelector("#container").style.borderTop =
    "3px solid " + (successfulParse ? "green" : "red");

  updateAlerts();

  ed.onDidChangeModelContent((e) => {
    let now = performance.now();
    env.updateFile("model.shadeup", ed.getValue() + "\n");
    let successfulParse = env.evaluate("model.shadeup");
    document.querySelector("#container").style.borderTop =
      "3px solid " + (successfulParse ? "green" : "red");
    edjs.setValue(env.generateFile("model.shadeup"));

    document.querySelector("#ast").innerHTML = env.getAst("model.shadeup");

    updateAlerts();

    console.log(`took ${performance.now() - now}ms`);
  });
})();

// const worker = new Worker(
//   new URL("./src/serverWorker.ts", window.location.href).href,
//   { type: "module" }
// );
// const reader = new BrowserMessageReader(worker);
// const writer = new BrowserMessageWriter(worker);
// const languageClient = createLanguageClient({ reader, writer });
// languageClient.start();
// console.log(worker);

// reader.onClose(() => languageClient.stop());
