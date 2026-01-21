"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = activate;
exports.deactivate = deactivate;
const vscode_1 = require("vscode");
const node_1 = require("vscode-languageclient/node");
let client;
function activate(context) {
    // Get the server path from configuration or use default
    const config = vscode_1.workspace.getConfiguration("vfp");
    let serverPath = config.get("server.path");
    if (!serverPath) {
        // Try to find vfp-lsp in PATH or use bundled version
        serverPath = "vfp-lsp";
    }
    // Server options - run the vfp-lsp executable
    const serverOptions = {
        run: {
            command: serverPath,
            transport: node_1.TransportKind.stdio,
        },
        debug: {
            command: serverPath,
            transport: node_1.TransportKind.stdio,
        },
    };
    // Client options
    const clientOptions = {
        // Register for VFP documents
        documentSelector: [{ scheme: "file", language: "vfp" }],
        synchronize: {
            // Notify the server about file changes to .prg files in the workspace
            fileEvents: vscode_1.workspace.createFileSystemWatcher("**/*.prg"),
        },
    };
    // Create the language client
    client = new node_1.LanguageClient("vfp-lsp", "Visual FoxPro Language Server", serverOptions, clientOptions);
    // Register restart command
    const restartCommand = vscode_1.commands.registerCommand("vfp.restartServer", async () => {
        if (client) {
            await client.stop();
            await client.start();
            vscode_1.window.showInformationMessage("VFP Language Server restarted");
        }
    });
    context.subscriptions.push(restartCommand);
    // Start the client (this also starts the server)
    client.start();
}
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
//# sourceMappingURL=extension.js.map