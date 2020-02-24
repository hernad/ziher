/*
  License: GPL 2.0
  Credits: source code based on "https://github.com/APerricone/harbourCodeExtension"
  */

var vscode = require('vscode');
var client = require('vscode-languageclient');

// reuse the bracket-match style

var decoration;

/** @type{client.LanguageClient} */
var client;

function activate(context, zhCli) {
	client = zhCli;
	decoration = vscode.window.createTextEditorDecorationType({
		borderStyle: 'solid',
		borderWidth: '1px',
		borderColor: new vscode.ThemeColor("editorBracketMatch.border"),
		backgroundColor: new vscode.ThemeColor("editorBracketMatch.background")
	});
	vscode.window.onDidChangeTextEditorSelection((e) => zh_decorator_show_groups(e));
}

function zh_decorator_show_groups(evt) {

	var configZiher = vscode.workspace.getConfiguration('ziher');
	
	if (!configZiher.decorator) 
	    return;

	var editor = evt.textEditor;

	if (!editor)
		return;

	if (!editor.document)
		return;

	if (editor.document.languageId != "ziher")
		return;

	if (evt.selections.length != 1) {
		evt.textEditor.setDecorations(decoration, []);
		return;
	}

	var sel = evt.selections[0];
	client.sendRequest("groupAtPosition",
		{
			textDocument: {
				uri: editor.document.uri.toString()
			}, 
			sel: sel
		})
		.then(ranges => {
			var places = [];
			for (let k = 0; k < ranges.length; k++) {
				const range = ranges[k];
				places.push(
					{ 
						range: new vscode.Range(range.line, range.startCol, range.line, range.endCol) 
					});
			}

			evt.textEditor.setDecorations(decoration, places);
		})
}

exports.activate = activate;
