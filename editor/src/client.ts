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
import "monaco-editor/esm/vs/editor/editor.all.js";

import ShadeupEnvironment from "./shadeup.js";

import MonarchDef from "./monarch.js";

// support all editor features
import "monaco-editor/esm/vs/editor/standalone/browser/accessibilityHelp/accessibilityHelp.js";
import "monaco-editor/esm/vs/editor/standalone/browser/inspectTokens/inspectTokens.js";
import "monaco-editor/esm/vs/editor/standalone/browser/iPadShowKeyboard/iPadShowKeyboard.js";
import "monaco-editor/esm/vs/editor/standalone/browser/quickAccess/standaloneHelpQuickAccess.js";
import "monaco-editor/esm/vs/editor/standalone/browser/quickAccess/standaloneGotoLineQuickAccess.js";
import "monaco-editor/esm/vs/editor/standalone/browser/quickAccess/standaloneGotoSymbolQuickAccess.js";
import "monaco-editor/esm/vs/editor/standalone/browser/quickAccess/standaloneCommandsQuickAccess.js";
import "monaco-editor/esm/vs/editor/standalone/browser/quickInput/standaloneQuickInputService.js";
import "monaco-editor/esm/vs/editor/standalone/browser/referenceSearch/standaloneReferenceSearch.js";
import "monaco-editor/esm/vs/editor/standalone/browser/toggleHighContrast/toggleHighContrast.js";

import * as monaco from "monaco-editor/esm/vs/editor/editor.api.js";

import { buildWorkerDefinition } from "monaco-editor-workers";

import {
  MonacoLanguageClient,
  CloseAction,
  ErrorAction,
  MonacoServices,
  MessageTransports,
} from "monaco-languageclient";
import {
  BrowserMessageReader,
  BrowserMessageWriter,
} from "vscode-languageserver-protocol/browser.js";
import { StandaloneServices } from "vscode/services";
import getMessageServiceOverride from "vscode/service-override/messages";

StandaloneServices.initialize({
  ...getMessageServiceOverride(document.body),
});

buildWorkerDefinition("dist", new URL("", window.location.href).href, false);

// register Monaco languages
monaco.languages.register({
  id: "plaintext",
  extensions: [".txt"],
  aliases: ["PLAINTEXT", "plaintext"],
  mimetypes: ["text/plain"],
});

// create Monaco editor
const editorText = `import test from "test";`;
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

// monaco.languages.register

monaco.languages.setMonarchTokensProvider("shadeup", MonarchDef);

function createLanguageClient(
  transports: MessageTransports
): MonacoLanguageClient {
  return new MonacoLanguageClient({
    name: "Sample Language Client",
    clientOptions: {
      // use a language id as a document selector
      documentSelector: [{ language: "shadeup" }],
      // disable the default error handler
      errorHandler: {
        error: () => ({ action: ErrorAction.Continue }),
        closed: () => ({ action: CloseAction.DoNotRestart }),
      },
    },
    // create a language client connection to the server running in the web worker
    connectionProvider: {
      get: () => {
        return Promise.resolve(transports);
      },
    },
  });
}

// install Monaco language client services
MonacoServices.install();

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
  env.updateFile("model.shadeup", ed.getValue());
  env.evaluate("model.shadeup");

  updateAlerts();

  ed.onDidChangeModelContent((e) => {
    let now = performance.now();
    env.updateFile("model.shadeup", ed.getValue() + "\n");
    env.evaluate("model.shadeup");

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
