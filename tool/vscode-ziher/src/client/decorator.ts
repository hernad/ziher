/*
  License: GPL 2.0
  Credits: source code based on "https://github.com/APerricone/harbourCodeExtension"
  */

import  * as vscode from 'vscode';
//const langClient = require('vscode-languageclient');
import * as langClient from 'vscode-languageclient';


// reuse the bracket-match style

let decoration;


function activate(context, zhLangClient: langClient.LanguageClient) {

	decoration = vscode.window.createTextEditorDecorationType({
		borderStyle: 'solid',
		borderWidth: '1px',
		borderColor: new vscode.ThemeColor("editorBracketMatch.border"),
		backgroundColor: new vscode.ThemeColor("editorBracketMatch.background")
	});
	
	vscode.window.onDidChangeTextEditorSelection( ( evt ) => {

		let configZiher = vscode.workspace.getConfiguration('ziher');

		if (!configZiher.decorator)
			return;

		let editor = evt.textEditor;

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

		let sel = evt.selections[0];
		
		zhLangClient.sendRequest("groupAtPosition",
			{
				textDocument: {
					uri: editor.document.uri.toString()
				},
				sel: sel
			})
			.then( (ranges: any) => {
				let places = [];
				for (let k = 0; k < ranges.length; k++) {
					const range = ranges[k];
					places.push(
						{
							range: new vscode.Range(range.line, range.startCol, range.line, range.endCol)
						});
				}
				evt.textEditor.setDecorations(decoration, places);
			})
			
	});
}

exports.activate = activate;
