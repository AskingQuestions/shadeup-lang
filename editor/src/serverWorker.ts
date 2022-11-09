/* --------------------------------------------------------------------------------------------
 * Copyright (c) 2018-2022 TypeFox GmbH (http://www.typefox.io). All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import {
  createConnection,
  BrowserMessageReader,
  BrowserMessageWriter,
} from "vscode-languageserver/browser.js";

import { default as AnsiUp } from "ansi_up";

import {
  Color,
  ColorInformation,
  Range,
  InitializeParams,
  InitializeResult,
  ServerCapabilities,
  CompletionParams,
  TextDocuments,
  ColorPresentation,
  TextEdit,
  TextDocumentIdentifier,
  CancellationToken,
  Diagnostic,
  TextDocumentSyncKind,
  DiagnosticSeverity,
} from "vscode-languageserver";
import { TextDocument } from "vscode-languageserver-textdocument";

import ShadeupEnvironment from "./shadeup";

// This is an example copied as is from here:
// https://github.com/microsoft/vscode-extension-samples/blob/main/lsp-web-extension-sample/server/src/browserServerMain.ts
// the only addition is the following line:
declare const self: DedicatedWorkerGlobalScope;

console.log("running server lsp-web-extension-sample");

/* browser specific setup code */

const messageReader = new BrowserMessageReader(self);
const messageWriter = new BrowserMessageWriter(self);

const connection = createConnection(messageReader, messageWriter);

/* from here on, all code is non-browser specific and could be shared with a regular extension */

let shadeup = new ShadeupEnvironment();

shadeup.load();

connection.onInitialize((_params: InitializeParams): InitializeResult => {
  const capabilities: ServerCapabilities = {
    textDocumentSync: TextDocumentSyncKind.Incremental,
    colorProvider: {}, // provide a color providr
    completionProvider: {
      resolveProvider: true,
      triggerCharacters: [".", "("],
    },
    documentHighlightProvider: true,
    hoverProvider: true,
    documentSymbolProvider: true,
  };
  return { capabilities };
});

// Track open, change and close text document events
const documents = new TextDocuments(TextDocument);
documents.listen(connection);

// Register providers
connection.onDocumentColor((params) =>
  getColorInformation(params.textDocument)
);
connection.onColorPresentation((params) =>
  getColorPresentation(params.color, params.range)
);

connection.onDocumentHighlight((params) => {
  const document = documents.get(params.textDocument.uri);
  console.log(params);
  if (document) {
    return [];
  }
  return [];
});

connection.onHover((params) => {
  const document = documents.get(params.textDocument.uri);
  console.log(params);
  if (document) {
    return {
      contents: `<span style="color:#ffff00;">22222222222222</span>`,
      supportHtml: true,
      isTrusted: true,
    };
  }
  return null;
});

// connection.onDidChangeTextDocument((params) => {
//   // A text document did change. This event is emitted
//   // when the text document first opened or when its content has changed.

//   const document = documents.get(params.textDocument.uri);

//   const text = document?.getText();
//   console.log(text, params);
//   shadeup.updateFile(params.textDocument.uri, text || "");
//   shadeup.evaulate(params.textDocument.uri);
// });

documents.onDidChangeContent(async (change) => {
  await shadeup.loaded();
  const text = change.document.getText();
  shadeup.updateFile(change.document.uri, text || "");
  shadeup.evaulate(change.document.uri);
  let diags: Diagnostic[] = [];
  for (let a of shadeup.getAlerts(change.document.uri)) {
    console.log(a);

    const ansi_up = new AnsiUp();

    let html = ansi_up.ansi_to_html(a.message);

    diags.push({
      supportHtml: true,
      isTrusted: true,
      severity: {
        0: DiagnosticSeverity.Error,
        1: DiagnosticSeverity.Warning,
        2: DiagnosticSeverity.Information,
      }[a.level],
      range: {
        start: {
          line: a.location.start_line_and_column[0] - 1,
          character: a.location.start_line_and_column[1],
        },
        end: {
          line: a.location.end_line_and_column[0] - 1,
          character: a.location.end_line_and_column[1],
        },
      },
      message: `<span style="color:#ffff00;">22222222222222</span>`,
      source: "shadeup",
    });
    console.log(diags);
  }

  connection.sendDiagnostics({
    uri: change.document.uri,
    diagnostics: diags,
  });
});

connection.onCompletion((params: CompletionParams) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) {
    return;
  }
  const offset = document.offsetAt(params.position);
  const text = document.getText();
  return [
    {
      label: "label",
      kind: 1,
      detail: "detail",
      documentation: "documentation",
      sortText: "sortText",
      filterText: "filterText",
      insertText: "insertText",
    },
  ];
});

connection.onCompletionResolve((item) => {
  return item;
});
// Listen on the connection
connection.listen();

const colorRegExp = /#([0-9A-Fa-f]{6})/g;

function getColorInformation(textDocument: TextDocumentIdentifier) {
  const colorInfos: ColorInformation[] = [];

  const document = documents.get(textDocument.uri);
  if (document) {
    const text = document.getText();

    colorRegExp.lastIndex = 0;
    let match;
    while ((match = colorRegExp.exec(text)) != null) {
      const offset = match.index;
      const length = match[0].length;

      const range = Range.create(
        document.positionAt(offset),
        document.positionAt(offset + length)
      );
      const color = parseColor(text, offset);
      colorInfos.push({ color, range });
    }
  }

  return colorInfos;
}

function getColorPresentation(color: Color, range: Range) {
  const result: ColorPresentation[] = [];
  const red256 = Math.round(color.red * 255);
  const green256 = Math.round(color.green * 255);
  const blue256 = Math.round(color.blue * 255);

  function toTwoDigitHex(n: number): string {
    const r = n.toString(16);
    return r.length !== 2 ? "0" + r : r;
  }

  const label = `#${toTwoDigitHex(red256)}${toTwoDigitHex(
    green256
  )}${toTwoDigitHex(blue256)}`;
  result.push({ label, textEdit: TextEdit.replace(range, label) });

  return result;
}

const enum CharCode {
  Digit0 = 48,
  Digit9 = 57,

  A = 65,
  F = 70,

  a = 97,
  f = 102,
}

function parseHexDigit(charCode: CharCode): number {
  if (charCode >= CharCode.Digit0 && charCode <= CharCode.Digit9) {
    return charCode - CharCode.Digit0;
  }
  if (charCode >= CharCode.A && charCode <= CharCode.F) {
    return charCode - CharCode.A + 10;
  }
  if (charCode >= CharCode.a && charCode <= CharCode.f) {
    return charCode - CharCode.a + 10;
  }
  return 0;
}

function parseColor(content: string, offset: number): Color {
  const r =
    (16 * parseHexDigit(content.charCodeAt(offset + 1)) +
      parseHexDigit(content.charCodeAt(offset + 2))) /
    255;
  const g =
    (16 * parseHexDigit(content.charCodeAt(offset + 3)) +
      parseHexDigit(content.charCodeAt(offset + 4))) /
    255;
  const b =
    (16 * parseHexDigit(content.charCodeAt(offset + 5)) +
      parseHexDigit(content.charCodeAt(offset + 6))) /
    255;
  return Color.create(r, g, b, 1);
}
