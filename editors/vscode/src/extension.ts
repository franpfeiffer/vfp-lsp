import * as path from "path";
import { workspace, ExtensionContext, commands, window } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export function activate(context: ExtensionContext) {
  // Get the server path from configuration or use default
  const config = workspace.getConfiguration("vfp");
  let serverPath = config.get<string>("server.path");

  if (!serverPath) {
    // Try to find vfp-lsp in PATH or use bundled version
    serverPath = "vfp-lsp";
  }

  // Server options - run the vfp-lsp executable
  const serverOptions: ServerOptions = {
    run: {
      command: serverPath,
      transport: TransportKind.stdio,
    },
    debug: {
      command: serverPath,
      transport: TransportKind.stdio,
    },
  };

  // Client options
  const clientOptions: LanguageClientOptions = {
    // Register for VFP documents
    documentSelector: [{ scheme: "file", language: "vfp" }],
    synchronize: {
      // Notify the server about file changes to .prg files in the workspace
      fileEvents: workspace.createFileSystemWatcher("**/*.prg"),
    },
  };

  // Create the language client
  client = new LanguageClient(
    "vfp-lsp",
    "Visual FoxPro Language Server",
    serverOptions,
    clientOptions
  );

  // Register restart command
  const restartCommand = commands.registerCommand("vfp.restartServer", async () => {
    if (client) {
      await client.stop();
      await client.start();
      window.showInformationMessage("VFP Language Server restarted");
    }
  });
  context.subscriptions.push(restartCommand);

  // Start the client (this also starts the server)
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
