/*
  License: GPL 2.0
  Credits: source code based on "https://github.com/APerricone/harbourCodeExtension"
  */

var vscode = require('vscode');
var path = require('path');
var validation = require('./client/validation.js');
var decorator = require('./client/decorator.js');
var doc_creator = require('./client/doc_creator.js');
var client = require('vscode-languageclient');
var fs = require("fs");

//var diagnosticCollection;

function activate(context) {

	vscode.languages.setLanguageConfiguration('ziher', {
		indentationRules: {
			increaseIndentPattern: /^\s*((?:(?:static|init|exit)\s+)?(?:proc(?:e(?:d(?:u(?:r(?:e)?)?)?)?)?|func(?:t(?:i(?:o(?:n)?)?)?)?)|class|method|if|else(?:if)?|for|if|try|case|otherwise|(?:do\s+)?while|switch|begin)\b/i,
			decreaseIndentPattern: /^\s*(end\s*([a-z]*)?|next|else|elseif|return)\b/i
		}
	});
	
	validation.activate(context);
	
	var serverModule = context.asAbsolutePath( path.join('dist','zh_server'));

	var debugOptions = { 
		execArgv: [
			"--nolazy", 
			"--inspect-brk=21780"
	    ]
	};
	var serverOptions = {
		run : { 
			module: serverModule, 
			transport: client.TransportKind.ipc,
		},
		debug: { 
			module: serverModule, 
			transport: client.TransportKind.ipc , 
			options: debugOptions 
		}
	} 
	var clientOptions = {
		documentSelector: [
			'ziher'
	    ],
		synchronize: {
			configurationSection: [
				'ziher',
				'search',
				'editor'
			]
		}
	}
	var langCli = new client.LanguageClient('ZiherServer', 'ZHLangServer', serverOptions, clientOptions);
	context.subscriptions.push(langCli.start());
	vscode.commands.registerCommand('ziher.getdbgcode', zh_get_debugger_code);
	//vscode.languages.registerFoldingRangeProvider(['ziher'], new decorator.HBProvider());
	
	doc_creator.activate(context, langCli);
	decorator.activate(context, langCli);
}	

function zh_get_debugger_code() {
	fs.readFile(path.resolve(__dirname, path.join('..', 'ziher','dbg_lib.zh')),(err,data) =>
    {
        if(!err)
			vscode.workspace.openTextDocument({
				content: data.toString(), 
				language: 'ziher'}).then(doc => {
					vscode.window.showTextDocument(doc);
				})
    });
}

function deactivate() {
	 validation.deactivate();
}

exports.activate = activate;
exports.deactivate = deactivate;

