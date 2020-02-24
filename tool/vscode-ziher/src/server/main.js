/*
  License: GPL 2.0
  Credits: source code based on "https://github.com/APerricone/harbourCodeExtension"
  */


var zhProvider = require('./zh_provider.js');
var server = require('vscode-languageserver')
var fs = require("fs");
var path = require("path");
var Uri = require("vscode-uri").default;

// Create a connection for the server. The connection uses Node's IPC as a transport
var connection = server.createConnection(
    new server.IPCMessageReader(process),
    new server.IPCMessageWriter(process)
);

const DOT_ZIHER_EXT = '.zh';
const DOT_ZIHER_HEADER_EXT = '.zhh';

const DOT_C_EXT = '.c';
const DOT_C_HEADER_EXT = '.h';

/** @type {Array<string>} */
var workspaceRoots;

/** @type {Array<string>} */
var aIncludeDirs;

/** @type {number} */
var workspaceDepth;

/** @type {boolean} */
var wordBasedSuggestions;

/** @type {Object.<string, provider.ZhLangProvider>} */
var oFiles;

/** @type {Object.<string, provider.ZhLangProvider>} */
var oProviderIncludes;

/** the list of documentation ziher functions
 * @type {Array<object>} */
var aFunctions;

/** the list of undocumentation ziher functions
 * @type {Array<string>} */
var aFunctionsMissing

/**
 * @typedef dbInfo
 * @property {string} dbInfo.name the name to show
 * @property {fieldInfo[]} dbInfo.fields the fields found for the database
 *
 * @typedef fieldInfo
 * @property {string} fieldInfo.name the name to show
 * @property {string[]} fieldInfo.files the list of files where the field is found
 */

/** @type {Object.<string, dbInfo>} every key is the lowercase name of db */
var oDbfs;

/** @type {boolean} */
var canLocationLink;

/** @type {boolean} */
var lineFoldingOnly;

var aKeywords = [
    "function", "procedure", "return",
    "if", "else", "elseif", "end if",
    "end while", "end case", "end do", "end switch", "end class", "end sequence",
    "do while", "case", "switch", "endcase", "otherwise", "default",
    "for", "for each", "to", "in", "next",
    "exit", "loop", "try", "catch", "finally",
    "begin sequence", "begin sequence with",
    "recover", "recover using"]

/*
    every database containts a name (the text before the ->)
    and a list of field, objects with name (the text after the ->)
    and a files, array of string with the file where found the db.name->field.name
*/

connection.onInitialize(params => {

    canLocationLink = false;
    if (params.capabilities.textDocument &&
        params.capabilities.textDocument.declaration &&
        params.capabilities.textDocument.declaration.linkSupport)
        canLocationLink = true;

    lineFoldingOnly = true;

    if (params.capabilities.textDocument &&
        params.capabilities.textDocument.foldingRange &&
        lineFoldingOnly in params.capabilities.textDocument.foldingRange)
        lineFoldingOnly = params.capabilities.textDocument.foldingRange.lineFoldingOnly;

    if (params.capabilities.workspace && params.capabilities.workspace.workspaceFolders && params.workspaceFolders) {
        workspaceRoots = [];
        for (var i = 0; i < params.workspaceFolders.length; i++) {
            if (params.workspaceFolders[i].uri)
                workspaceRoots.push(params.workspaceFolders[i].uri)
        }
    } else {
        workspaceRoots = [params.rootUri];
        if (!workspaceRoots[0] && params.rootPath) {
            if (path.sep == "\\") //window
                workspaceRoots = ["file://" + encodeURI(params.rootPath.replace(/\\/g, "/"))];
            else
                workspaceRoots = ["file://" + encodeURI(params.rootPath)];
        }
        if (!workspaceRoots[0]) workspaceRoots = [];
    }

    // console.log(`sever __dirname=${__dirname}`); -> vscode-ziher/dist
    fs.readFile(path.join(__dirname, 'zh_docs.json'), "utf8", (err, data) => {
        if (!err)
            aFunctions = JSON.parse(data);
    });

    fs.readFile(path.join(__dirname, 'zh_docs.missing'), "utf8", (err, data) => {
        if (!err)
            aFunctionsMissing = data.split(/\r\n{1,2}/g)
    });

    return {
        capabilities: {
            documentSymbolProvider: true,
            workspaceSymbolProvider: true,
            definitionProvider: true,
            // declarationProvider: true,
            signatureHelpProvider: {
                triggerCharacters: ['(']
            },
            completionProvider: {
                resolveProvider: false,
                triggerCharacters: ['>', '<', '"']
            },
            // Tell the client that the server works in FULL text document sync mode
            textDocumentSync: 1,
            workspace: {
                supported: true
            },
            hoverProvider: true,
            foldingRangeProvider: true
        }
    }
});

/*
connection.workspace.onDidChangeWorkspaceFolders(params=>{
    var i=0;
})
*/

connection.onDidChangeConfiguration(params => {

    //var searchExclude = params.settings.search.exclude;
    // minimatch
    aIncludeDirs = params.settings.ziher.extraIncludePaths;
    aIncludeDirs.splice(0, 0, ".")
    workspaceDepth = params.settings.ziher.workspaceDepth;
    wordBasedSuggestions = params.settings.editor.wordBasedSuggestions
    parse_workspace();

})

function parse_zh_dir(dir, onlyHeader, depth, subirPaths) {

    if (!subirPaths)
        subirPaths = [];

    fs.readdir(dir, function (err, aFiles) {
        if (aFiles == undefined)
            return;

        for (var i = 0; i < aFiles.length; i++) {

            var fileName = aFiles[i];
            var completePath = path.join(dir, fileName);
            var info = fs.statSync(completePath);
            if (info.isFile()) {
                var ext = path.extname(fileName).toLowerCase();
                if (onlyHeader && ext != DOT_ZIHER_HEADER_EXT && ext != DOT_C_HEADER_EXT) {
                    continue;
                }
                var modeCLang = (ext.startsWith(DOT_C_EXT) && ext != DOT_ZIHER_HEADER_EXT) || ext == DOT_C_HEADER_EXT
                if (modeCLang) {
                    var ziherFile = path.basename(fileName)
                    var pos = ziherFile.lastIndexOf(".");
                    ziherFile = ziherFile.substr(0, pos < 0 ? ziherFile.length : pos) + DOT_ZIHER_EXT;
                    if (subirPaths.findIndex((v) => v.indexOf(ziherFile) >= 0) >= 0)
                        continue;
                }
                if (ext == DOT_ZIHER_EXT || ext == DOT_ZIHER_HEADER_EXT || modeCLang) {

                    subirPaths.push(completePath);
                    var fileUri = Uri.file(completePath);
                    var provider = new zhProvider.ZhLangProvider(true);
                    provider.parseFile(completePath, fileUri.toString(), modeCLang).then(
                        provider => {
                            zh_update_file(provider)
                        }
                    )
                }
            } else if (info.isDirectory() && depth > 0) {
                parse_zh_dir(path.join(dir, fileName), onlyHeader, depth - 1, subirPaths);
            }
        }
    });
}

function parse_workspace() {

    oDbfs = {};
    oFiles = {};
    oProviderIncludes = {};

    for (var i = 0; i < workspaceRoots.length; i++) {
        // other scheme of uri unsupported
        if (workspaceRoots[i] == null)
            continue;
        /** @type {vscode-uri.default} */
        var uri = Uri.parse(workspaceRoots[i]);
        if (uri.scheme != "file") return;
        parse_zh_dir(uri.fsPath, false, workspaceDepth);
    }
}

/**
 * Update a file in the workspace
 * @param {provider.ZhLangProvider} provider
 */
function zh_update_file(provider) {

    var doc = provider.currentDocument;
    var ext = path.extname(provider.currentDocument).toLowerCase();
    if (ext != DOT_ZIHER_EXT) {
        oFiles[doc] = provider;
        return;
    }

    if (doc in oFiles) {
        for (var dbf in oDbfs) {
            for (var field in oDbfs[dbf].fields) {
                var idx = oDbfs[dbf].fields[field].files.indexOf(doc);
                if (idx >= 0) {
                    oDbfs[dbf].fields[field].files.splice(idx, 1);
                    if (oDbfs[dbf].fields[field].files.length == 0) {
                        delete oDbfs[dbf].fields[field];
                    }
                }
            }
            if (Object.keys(oDbfs[dbf].fields).length == 0) {
                delete oDbfs[dbf];
            }
        }
    }

    oFiles[doc] = provider;
    for (var dbf in provider.aDbfs) {
        var providerDbf = provider.aDbfs[dbf];
        if (!(dbf in oDbfs))
            oDbfs[dbf] = {
                name: providerDbf.name,
                fields: {}
            };

        var oDbf = oDbfs[dbf];
        for (var field in providerDbf.fields) {
            if (!(field in oDbf.fields))
                oDbf.fields[field] = {
                    name: providerDbf.fields[field],
                    files: [doc]
                };
            else {
                var idx = oDbf.fields[field].files.indexOf(doc);
                if (idx < 0)
                    oDbf.fields[field].files.push(doc);
            }
        }
    }
    zh_add_includes(path.dirname(doc), provider.aIncludes);
}

function zh_add_includes(startPath, aProviderIncludes) {

    if (aProviderIncludes.length == 0)
        return;

    if (startPath.startsWith("file:///"))
        startPath = Uri.parse(startPath).fsPath;

    function zh_find_include_dir_file_name(dir, fileName) {

        if (startPath && !path.isAbsolute(dir))
            dir = path.join(startPath, dir);

        if (!fs.existsSync(dir)) return false;

        if (fileName.length < 1)
            return false;

        var completePath = path.join(dir, fileName);
        if (!fs.existsSync(completePath))
            return false;

        var fileUri = Uri.file(completePath);
        var provider = new zhProvider.ZhLangProvider(true);
        provider.parseFile(completePath, fileUri.toString(), false).then(
            provider => {
                oProviderIncludes[fileName] = provider;
                zh_add_includes(dir, provider.aIncludes);
            }
        )
        return true;
    }

    for (var j = 0; j < aProviderIncludes.length; j++) {

        var cInclude = aProviderIncludes[j];
        if (cInclude in oProviderIncludes)
            continue;

        var lFound = false;
        for (var i = 0; i < workspaceRoots.length; i++) {

            // other scheme of uri unsupported
            /** @type {vscode-uri.default} */
            var uri = Uri.parse(workspaceRoots[i]);
            if (uri.scheme != "file")
                continue;

            lFound = zh_find_include_dir_file_name(uri.fsPath, cInclude);
            if (lFound)
                break;
        }

        if (lFound)
            continue;

        for (var i = 0; i < aIncludeDirs.length; i++) {
            lFound = zh_find_include_dir_file_name(aIncludeDirs[i], cInclude);
            if (lFound)
                break;
        }
    }
}

function zh_parse_include(startPath, includeName, addGlobal) {

    if (includeName.length == 0)
        return undefined;

    if (includeName in oProviderIncludes)
        return oProviderIncludes[includeName];

    function zh_find_include_dir(dir) {

        if (startPath && !path.isAbsolute(dir))
            dir = path.join(startPath, dir);

        if (!fs.existsSync(dir))
            return undefined;

        var test = path.join(dir, includeName);
        if (!fs.existsSync(test))
            return undefined;

        var provider = new zhProvider.ZhLangProvider();
        provider.parseString(fs.readFileSync(test).toString(), Uri.file(test).toString());
        if (addGlobal)
            oProviderIncludes[includeName] = provider;

        return provider;
    }

    for (var i = 0; i < workspaceRoots.length; i++) {
        // other scheme of uri unsupported
        /** @type {vscode-uri.default} */
        var uri = Uri.parse(workspaceRoots[i]);
        if (uri.scheme != "file")
            continue;
        var provider = zh_find_include_dir(uri.fsPath);
        if (provider)
            return provider;
    }

    for (var i = 0; i < aIncludeDirs.length; i++) {
        var provider = zh_find_include_dir(aIncludeDirs[i]);
        if (provider)
            return provider;
    }

}

function get_completition_item_kind_type(strKind, symbolKind) {

    if (symbolKind == undefined)
        symbolKind = true;

    switch (strKind) {
        case "class":
            return symbolKind ? server.SymbolKind.Class : server.CompletionItemKind.Class;
        case "method":
            return symbolKind ? server.SymbolKind.Method : server.CompletionItemKind.Method;
        case "data":
            return symbolKind ? server.SymbolKind.Property : server.CompletionItemKind.Property;
        case "function":
        case "procedure":
        case "function*":
        case "procedure*":
        case "c_func":
            return symbolKind ? server.SymbolKind.Function : server.CompletionItemKind.Function;
        case "local":
        case "static":
        case "public":
        case "private":
        case "param":
        case "memvar":
            return symbolKind ? server.SymbolKind.Variable : server.CompletionItemKind.Variable;
        case "field":
            return symbolKind ? server.SymbolKind.Field : server.CompletionItemKind.Field;
        case "define":
            return symbolKind ? server.SymbolKind.Constant : server.CompletionItemKind.Constant;
    }
    return 0;
}

/*
    -> SymbolInformation[] | DocumentSymbol[]
*/
connection.onDocumentSymbol((param) => {

    var doc = documents.get(param.textDocument.uri);

    /** @type {provider.ZhLangProvider} */
    var provider = get_document_provider(doc);

    /** @type {server.DocumentSymbol[]} */
    var aSymbols = [];
    for (var oFunc in provider.aFunctions) {

        /** @type {provider.ZhInfo} */
        var oZhInfo = provider.aFunctions[oFunc];

        if (oZhInfo.kind == "field")
            continue;

        if (oZhInfo.kind == "memvar")
            continue;

        var selRange = server.Range.create(oZhInfo.startLine, oZhInfo.startCol, oZhInfo.endLine, oZhInfo.endCol);
        if (oZhInfo.endLine != oZhInfo.startLine)
            selRange.end = server.Position.create(oZhInfo.startLine, 1000);

        var docSymbol = server.DocumentSymbol.create(
            oZhInfo.name,
            (oZhInfo.comment && oZhInfo.comment.length > 0 ? oZhInfo.comment.replace(/[\r\n]+/g, " ") : ""),
            get_completition_item_kind_type(oZhInfo.kind),
            server.Range.create(oZhInfo.startLine, oZhInfo.startCol, oZhInfo.endLine, oZhInfo.endCol),
            selRange
        );

        var aParent = aSymbols;
        if (oZhInfo.parent && oZhInfo.startLine <= oZhInfo.parent.endLine) {
            var zhInfoParent = oZhInfo.parent;
            var aNames = [];
            while (zhInfoParent) {
                if (zhInfoParent.kind == "method" && zhInfoParent.foundLike == "definition" && (!zhInfoParent.parent || zhInfoParent.startLine > zhInfoParent.parent.endLine)) {
                    aNames.push(zhInfoParent.parent.name + ":" + zhInfoParent.name);
                    break;
                } else
                    aNames.push(zhInfoParent.name);

                zhInfoParent = zhInfoParent.parent;
            }
            while (aNames.length > 0) {
                var n = aNames.pop();
                var i = aParent.findIndex((v) => (v.name == n));
                if (i >= 0) {
                    aParent = aParent[i];
                    if (!aParent.children)
                        aParent.children = [];
                    aParent = aParent.children;
                }
            }
        } else
            if (oZhInfo.kind == "method" && oZhInfo.parent)
                docSymbol.name = oZhInfo.parent.name + ":" + oZhInfo.name

        aParent.push(docSymbol);
    };
    return aSymbols;
});

function is_inside(word1, word2) {

    var ret = "";
    var i1 = 0;
    var lenMatch = 0, maxLenMatch = 0, minLenMatch = word1.length;
    for (var i2 = 0; i2 < word2.length; i2++) {
        if (word1[i1] == word2[i2]) {
            lenMatch++;
            if (lenMatch > maxLenMatch) maxLenMatch = lenMatch;
            ret += word1[i1];
            i1++;
            if (i1 == word1.length) {
                return ret;
            }
        } else {
            ret += "Z";
            if (lenMatch > 0 && lenMatch < minLenMatch)
                minLenMatch = lenMatch;
            lenMatch = 0;
        }
    }
    return undefined;
}

/*
private onWorkspaceSymbol(params: LSP.WorkspaceSymbolParams): 
     LSP.SymbolInformation[] 
*/

connection.onWorkspaceSymbol((param) => {

    var aSymbolInformation = [];
    var src = param.query.toLowerCase();
    var parent = undefined;

    var colon = src.indexOf(':');
    if (colon > 0) {
        parent = src.substring(0, colon);
        if (parent.endsWith("()")) parent = parent.substr(0, parent.length - 2);
        src = src.substr(colon + 1);
    }

    for (var file in oFiles) {

        var provider = oFiles[file];
        for (var fn in provider.aFunctions) {

            /** @type {provider.ZhInfo} */
            var zhInfo = provider.aFunctions[fn];
            if (zhInfo.kind != "class" && zhInfo.kind != "method" &&
                zhInfo.kind != "data" && zhInfo.kind != "public" &&
                zhInfo.kind != "define" &&
                !zhInfo.kind.startsWith("procedure") &&
                !zhInfo.kind.startsWith("function"))
                continue;

            // workspace symbols takes statics too
            if (src.length > 0 && !is_inside(src, zhInfo.nameCmp))
                continue;

            // public has parent, but they are visible everywhere
            if (parent && zhInfo.kind != "public" && (!zhInfo.parent || !is_inside(parent, zhInfo.parent.nameCmp)))
                continue;

            aSymbolInformation.push(
                server.SymbolInformation.create(
                    zhInfo.name,
                    get_completition_item_kind_type(zhInfo.kind, true),
                    server.Range.create(
                        zhInfo.startLine,
                        zhInfo.startCol,
                        zhInfo.endLine,
                        zhInfo.endCol
                    ),
                    file,
                    zhInfo.parent ? zhInfo.parent.name : ""
                )
            );
            if (aSymbolInformation.length == 100)
                return aSymbolInformation;
        }
    }
    return aSymbolInformation;
});

function get_word(params, withPrec) {

    var doc = documents.get(params.textDocument.uri);
    var pos = doc.offsetAt(params.position);
    var delta = 20;
    var word, prec;
    //var allText = doc.getText();
    var r = /\b[a-z_][a-z0-9_]*\b/gi
    while (true) {
        r.lastIndex = 0;
        //var text = allText.substr(Math.max(pos-delta,0),delta+delta)
        var text = doc.getText(server.Range.create(doc.positionAt(Math.max(pos - delta, 0)), doc.positionAt(pos + delta)));
        var txtPos = pos < delta ? pos : delta;
        while (word = r.exec(text)) {
            if (word.index <= txtPos && word.index + word[0].length >= txtPos)
                break;
        }
        if (!word)
            return [];
        if (word.index != 0 && (word.index + word[0].length) != (delta + delta)) {
            prec = text[word.index - 1];
            break;
        }
        delta += 10;
    }
    var worldPos = pos - delta + word.index;
    word = word[0];

    return withPrec ? [word, prec, worldPos] : word;
}

connection.onDefinition((params) => {

    var doc = documents.get(params.textDocument.uri);
    var line = doc.getText(server.Range.create(params.position.line, 0, params.position.line, 100));

    var include = /^\s*#include\s+[<"]([^>"]*)/i.exec(line);
    if (include !== null) {
        var startPath = undefined;
        if (params.textDocument.uri && params.textDocument.uri.startsWith("file")) {
            startPath = path.dirname(Uri.parse(params.textDocument.uri).fsPath);
        }
        var pos = include[0].indexOf(include[1]);
        return definition_files(include[1], startPath, server.Range.create(params.position.line, pos, params.position.line, pos + include[1].length));
    }
    var word = get_word(params, true);
    if (word.length == 0)
        return undefined;

    var dest = [];
    var thisDone = false;
    var prec = word[1];
    var className;
    var pos = word[2];
    if (prec == ':' && doc.getText(server.Range.create(doc.positionAt(Math.max(pos - 3, 0)), doc.positionAt(pos))) == "():") {
        var tmp = params.position;
        params.position = doc.positionAt(Math.max(pos - 3, 0));
        className = get_word(params).toLowerCase();
        params.position = tmp;
        var found = false;
        for (var file in oFiles) {
            if (file == doc.uri) thisDone = true;
            var provider = oFiles[file];
            for (var fn in provider.aFunctions) {
                /** @type {provider.ZhInfo} */
                var info = provider.aFunctions[fn];
                if (info.kind != 'class')
                    continue
                if (info.nameCmp == className) {
                    found = true;
                    break;
                }
            }
        }
        var providerThis
        if (!thisDone && !found) {
            providerThis = get_document_provider(doc);
            for (var fn in provider.aFunctions) { //if (pp.aFunctions.hasOwnProperty(fn)) {
                /** @type {provider.ZhInfo} */
                var info = provider.aFunctions[fn];
                if (info.kind != 'class')
                    continue;
                if (info.nameCmp == className) {
                    found = true;
                    break;
                }
            }

        }
        if (!found) className = undefined;
    }

    word = word[0].toLowerCase();

    function zh_do_provider(provider, file) {

        for (var fn in provider.aFunctions) {

            /** @type {provider.ZhInfo} */
            var oZhInfo = provider.aFunctions[fn];

            if (oZhInfo.foundLike != "definition")
                continue;

            if (oZhInfo.nameCmp != word)
                continue;

            if (oZhInfo.kind.endsWith("*") && file != doc.uri)
                continue;

            if (oZhInfo.kind == 'static' && file != doc.uri)
                continue;

            if (oZhInfo.kind == 'data' || oZhInfo.kind == 'method') {
                if (className && className != oZhInfo.parent.nameCmp)
                    continue;
            }

            if (oZhInfo.kind == 'local' || oZhInfo.kind == 'param') {
                if (file != doc.uri)
                    continue;
                var parent = oZhInfo.parent;
                if (parent) {
                    if (parent.startLine > params.position.line)
                        continue;
                    if (parent.endLine < params.position.line)
                        continue;
                }
            }
            dest.push(server.Location.create(file,
                server.Range.create(oZhInfo.startLine, oZhInfo.startCol,
                    oZhInfo.endLine, oZhInfo.endCol)));
        }
    }

    for (var file in oFiles) {
        if (file == doc.uri) thisDone = true;
        zh_do_provider(oFiles[file], file);
    }

    var providerThis;
    if (!thisDone) {
        providerThis = get_document_provider(doc);
        zh_do_provider(providerThis, doc.uri);
    } else
        providerThis = oFiles[doc.uri];

    var aIncludes = providerThis.aIncludes;
    var i = 0;
    var startDir = path.dirname(Uri.parse(doc.uri).fsPath);
    while (i < aIncludes.length) {
        provider = zh_parse_include(startDir, aIncludes[i], thisDone);
        if (provider) {
            zh_do_provider(provider, provider.currentDocument)
            for (var j = 0; j < provider.aIncludes; j++) {
                if (aIncludes.indexOf(provider.aIncludes[j]) < 0)
                    aIncludes.push(provider.aIncludes[j]);
            }
        }
        i++;
    }

    return dest;
})

connection.onSignatureHelp((params) => {

    var doc = documents.get(params.textDocument.uri);
    var pos = doc.offsetAt(params.position) - 1;
    /** @type {string} */
    var text = doc.getText();

    // backwards find (
    pos = find_bracket(text, pos, -1, "(")
    if (pos === undefined)
        return pos;

    // Get parameter position
    var endPos = doc.offsetAt(params.position)
    var nC = count_parameter(text.substr(pos + 1, endPos - pos - 1), doc.offsetAt(params.position) - pos - 1)

    // Get the word
    pos--;
    var rge = /[0-9a-z_]/i;
    var word = "", className = undefined;
    while (rge.test(text[pos])) {
        word = text[pos] + word;
        pos--;
    }
    word = word.toLowerCase();

    // special case for new, search the class name
    var prec = text.substring(pos - 2, pos + 1);

    if (prec == "():") {
        pos -= 3;
        className = "";
        while (rge.test(text[pos])) {
            className = text[pos] + className;
            pos--;
        }
        className = className.toLowerCase();
    }

    var signatures = [].concat(get_workspace_help_signatures(word, doc, className, nC));
    if (signatures.length == 0 && className !== undefined) {
        signatures = [].concat(get_workspace_help_signatures(word, doc, undefined, nC));
    }
    signatures = signatures.concat(getStdHelp(word, nC));

    return { 
        signatures: signatures, 
        activeParameter: nC 
    }
})

/**
 *
 * @param {String} text
 * @param {Number} pos
 * @param {Number} dir
 * @param {String} bracket
 */
function find_bracket(text, pos, dir, bracket) {

    var nP = 0;
    var str;

    while (nP != 0 || text[pos] != bracket || str != undefined) {
        if (pos < 0)
            return undefined;
        if (pos >= text.length)
            return undefined;
        if (str) {
            if (text[pos] == str)
                str = undefined;
        } else {
            switch (text[pos]) {
                case '(':
                    nP--;
                    break;
                case ')':
                    nP++;
                    break;
                case '[':
                    if (dir > 0)
                        str = ']';
                    break
                case ']':
                    if (dir < 0)
                        str = '[';
                    break
                case '{':
                    if (dir > 0)
                        str = '}';
                    break
                case '}':
                    if (dir < 0)
                        str = '{';
                    break;
                case '"':
                    str = '"';
                    break;
                case "'":
                    str = "'";
                    break;
                case '\n':
                    var nSpace = 1;
                    while (text[pos - nSpace] != '\n')
                        nSpace++;
                    var thisLine = text.substr(pos - nSpace + 1, nSpace)
                    thisLine = thisLine.replace(/\/\/[^\n]*\n/, "\n")
                    thisLine = thisLine.replace(/&&[^\n]*\n/, "\n")
                    thisLine = thisLine.replace(/\s+\n/, "\n")
                    if (thisLine[thisLine.length - 2] == ';')
                        break;

                    return undefined;
                //break;
            }
        }
        pos += dir;
    }
    return pos
}

/**
 *
 * @param {String} txt The text where count the parameter
 * @param {Number} position Position of cursor
 */
function count_parameter(txt, position) {
    var i = 0;
    while (true) {
        i++;
        var filter = undefined;
        switch (i) {
            case 1: filter = /;\s*\r?\n/g; break;  // new line with ;
            case 2: filter = /'[^']*'/g; break; // ' strings
            case 3: filter = /"[^"]*"/g; break; // " strings
            case 4: filter = /\[[^\[\]]*\]/g; break; // [] strings or array index
            case 5: filter = /{[^{}]*}/g; break; // {} array
            case 6: filter = /\([^\(\)]*\)/g; break; // couple of parenthesis
        }
        if (filter == undefined)
            break;
        do {
            var someChange = false
            txt = txt.replace(filter, function (matchString) {
                someChange = true;
                return Array(matchString.length + 1).join("X");
            })
        } while (someChange)
    }
    return (txt.substr(0, position).match(/,/g) || []).length
}

function get_workspace_help_signatures(word, doc, className, nC) {

    var aSignatures = [];
    var thisDone = false;

    function get_signature_from_info(provider, oZhInfo) {

        if ("zhDocIdx" in oZhInfo)
            return get_func_help_from_zh_info(provider.ziherDocs[oZhInfo.zhDocIdx]);

        var oSignature = {};
        if (oZhInfo.kind.startsWith("method"))
            if (oZhInfo.parent) {
                oSignature["label"] = oZhInfo.parent.name + ":" + oZhInfo.name;
                if (className && className != oZhInfo.parent.nameCmp)
                    return undefined;
            }
            else {
                oSignature["label"] = "??:" + oZhInfo.name;
                if (className)
                    return undefined;
            }
        else
            oSignature["label"] = oZhInfo.name;

        oSignature["label"] += "(";
        var aParameters = [];
        for (var iParam = iSign + 1; iParam < provider.aFunctions.length; iParam++) {

            /** @type {provider.ZhInfo} */
            var oZhInfo = provider.aFunctions[iParam];
            if (oZhInfo.parent == oZhInfo && oZhInfo.kind == "param") {
                var oParameter = { "label": oZhInfo.name };
                if (oZhInfo.comment && oZhInfo.comment.trim().length > 0)
                    oParameter["documentation"] = "<" + oZhInfo.name + "> " + oZhInfo.comment;

                aParameters.push(oParameter);
                if (!oSignature.label.endsWith("("))
                    oSignature.label += ", ";

                oSignature.label += oZhInfo.name;
            } else
                break;
        }

        oSignature["label"] += ")";
        oSignature["parameters"] = aParameters;
        if (oZhInfo.comment && oZhInfo.comment.trim().length > 0)
            oSignature["documentation"] = oZhInfo.comment;

        return oSignature;
    }

    for (var file in oFiles) {

        if (file == doc.uri)
            thisDone = true;

        var provider = oFiles[file];
        for (var iSign = 0; iSign < provider.aFunctions.length; iSign++) {

            /** @type {provider.ZhInfo} */
            var oZhInfo = provider.aFunctions[iSign];
            if (!oZhInfo.kind.startsWith("method") && !oZhInfo.kind.startsWith("procedure") && !oZhInfo.kind.startsWith("function"))
                continue;

            if (oZhInfo.nameCmp != word)
                continue;

            if (oZhInfo.kind.endsWith("*") && file != doc.uri)
                continue;

            var oSignature = get_signature_from_info(provider, oZhInfo);
            if (oSignature && oSignature["parameters"].length >= nC)
                aSignatures.push(oSignature);
        }
    }
    if (!thisDone) {

        var provider = get_document_provider(doc);

        for (var iSign = 0; iSign < provider.aFunctions.length; iSign++) {
            /** @type {provider.ZhInfo} */
            var oZhInfo = provider.aFunctions[iSign];
            if (!oZhInfo.kind.startsWith("method") && !oZhInfo.kind.startsWith("procedure") && !oZhInfo.kind.startsWith("function"))
                continue;

            if (oZhInfo.nameCmp != word)
                continue;

            var oSignature = get_signature_from_info(provider, oZhInfo);
            if (oSignature && oSignature["parameters"].length >= nC)
                aSignature.push(oSignature);
        }

    }
    return aSignatures;
}

function get_func_help_from_zh_info(doc) {

    var oSignature = {};
    oSignature["label"] = doc.label;
    oSignature["documentation"] = doc.documentation;
    var subParams = [];
    for (var iParam = 0; iParam < doc.arguments.length; iParam++) {
        subParams.push({
            "label": doc.arguments[iParam].label,
            "documentation": doc.arguments[iParam].documentation
        });
    }
    oSignature["parameters"] = subParams;
    return oSignature;
}

function getStdHelp(word, nC) {

    var aSignatures = [];
    for (var i = 0; i < aFunctions.length; i++) {
        if (aFunctions[i].name.toLowerCase() == word) {
            aSignatures.push(get_func_help_from_zh_info(aFunctions[i]));
        }
    }
    return aSignatures;
}

var documents = new server.TextDocuments();
documents.listen(connection);

documents.onDidChangeContent((e) => {
    var uri = Uri.parse(e.document.uri);
    if (uri.scheme != "file")
        return;
    var found = false;
    for (var i = 0; i < workspaceRoots.length; i++)
        if (e.document.uri.startsWith(workspaceRoots[i]))
            found = true;
    if (!found)
        return; //not include file outside the current workspace
    var ext = path.extname(uri.fsPath).toLowerCase();
    var cMode = (ext.startsWith(DOT_C_EXT) && ext != DOT_ZIHER_HEADER_EXT)
    if (ext == DOT_ZIHER_EXT || ext == DOT_ZIHER_HEADER_EXT || cMode) {
        var doGroups = false;
        if (uri in oFiles) doGroups = oFiles[uri].doGroups;
        var provider = parse_document(
            e.document,
            (p) => {
                p.cMode = cMode;
                p.doGroups = doGroups;
            }
        );
        zh_update_file(provider);
    }
})

/**
 *
 * @param {server.TextDocument} doc
 * @param {boolean} cMode
 * @returns {provider.ZhLangProvider}
 */
function parse_document(doc, onInit) {
    var provider = new zhProvider.ZhLangProvider(false);
    provider.Clear();
    provider.currentDocument = doc.uri;
    if (onInit != undefined) onInit(provider);
    for (var i = 0; i < doc.lineCount; i++) {
        provider.parse(doc.getText(server.Range.create(i, 0, i, 1000)));
    }
    provider.endParse();
    return provider;
}

/** @type {provider.ZhLangProvider} */
var lastDocOutsideWorkspaceProvider = { currentDocument: "" };

function get_document_provider(doc, checkGroup) {

    var provider;
    if (doc.uri in oFiles) {
        provider = oFiles[doc.uri]
        if (checkGroup && !provider.doGroups)
            provider = oFiles[doc.uri] = parse_document(doc, (p) => p.doGroups = true);
        return provider;
    }
    if (doc.uri == lastDocOutsideWorkspaceProvider.currentDocument) {
        provider = lastDocOutsideWorkspaceProvider;
        if (checkGroup && !provider.doGroups)
            provider = lastDocOutsideWorkspaceProvider = parse_document(doc, (p) => p.doGroups = true);
        return provider;
    }
    if (checkGroup)
        provider = parse_document(doc, (p) => p.doGroups = true);
    else
        provider = parse_document(doc);

    return provider;
}

connection.onCompletion(

    (param, cancelled) => {
        var doc = documents.get(param.textDocument.uri);
        var line = doc.getText(server.Range.create(param.position.line, 0, param.position.line, 1000));
        var include = line.match(/^\s*#include\s+[<"]([^>"]*)/i);
        var precLetter = doc.getText(server.Range.create(server.Position.create(param.position.line, param.position.character - 1), param.position));
        if (include !== null) {
            if (precLetter == '>') {
                return server.CompletionList.create([], false); // wrong call
            }
            var startPath = undefined;
            if (param.textDocument.uri && param.textDocument.uri.startsWith("file")) {
                startPath = path.dirname(Uri.parse(param.textDocument.uri).fsPath)
            }
            var includePos = line.lastIndexOf(include[1]);
            return completition_files(
                include[1],
                startPath,
                server.Range.create(server.Position.create(param.position.line, includePos),
                    server.Position.create(param.position.line, includePos + include[1].length - 1))
            );
        }
        var allText = doc.getText();
        var aCompletitions = [];
        var pos = doc.offsetAt(param.position) - 1
        // Get the word
        var rge = /[0-9a-z_]/i;
        var word = "", className = undefined;
        var provider = get_document_provider(doc);
        while (pos >= 0 && rge.test(allText[pos])) {
            word = allText[pos] + word;
            pos--;
        }
        word = word.toLowerCase();
        var precLetter = allText[pos];
        if (precLetter == '>') {
            if (allText[pos - 1] == '-') {
                precLetter = '->';
                aCompletitions = completition_dbf_fields(word, allText, pos, provider)
                if (aCompletitions.length > 0)
                    return server.CompletionList.create(aCompletitions, true); // put true because added all known field of this db
            } else {
                return server.CompletionList.create([], false); // wrong call
            }
        }

        var aDone = {}
        function check_add(label, kind, sort) {

            var labelLower = label.toLowerCase()
            if (labelLower in aDone)
                return;

            aDone[labelLower] = true;
            var sortLabel = is_inside(word, labelLower);
            if (sortLabel === undefined)
                return undefined;

            var oCompletionItem = server.CompletionItem.create(label);
            oCompletionItem.kind = kind;
            oCompletionItem.sortText = sort + sortLabel;
            aCompletitions.push(oCompletionItem);
            return oCompletionItem;
        }

        if (precLetter != '->' && precLetter != ':')
            precLetter = undefined;

        if (word.length == 0 && precLetter == undefined)
            return server.CompletionList.create(aCompletitions, false);

        if (!precLetter) {
            for (var dbName in oDbfs) {
                check_add(oDbfs[dbName].name, server.CompletionItemKind.Struct, "AAAA")
                if (cancelled.isCancellationRequested)
                    return server.CompletionList.create(aCompletitions, false);
            }
            if (provider) {
                for (var dbName in provider.aDbfs) {
                    check_add(provider.aDbfs[dbName].name, server.CompletionItemKind.Struct, "AAAA")
                    if (cancelled.isCancellationRequested)
                        return server.CompletionList.create(aCompletitions, false);
                }
            }
        }

        function get_completitions(provider, file) {

            for (var iSign = 0; iSign < provider.aFunctions.length; iSign++) {

                /** @type {provider.ZhInfo} */
                var oZhInfo = provider.aFunctions[iSign];

                if (word.length > 0 && !is_inside(word, oZhInfo.nameCmp))
                    continue;
                if (oZhInfo.endCol == param.position.character && oZhInfo.endLine == param.position.line && file == doc.uri)
                    continue;
                if (precLetter == '->' && oZhInfo.kind != "field")
                    continue;
                if (precLetter != '->' && oZhInfo.kind == "field")
                    continue;
                if (precLetter == ':' && oZhInfo.kind != "method" && oZhInfo.kind != "data")
                    continue;
                if (precLetter != ':' && (oZhInfo.kind == "method" || oZhInfo.kind == "data"))
                    continue;
                if (oZhInfo.kind == "function*" || oZhInfo.kind == "procedure*" || oZhInfo.kind == "static") {
                    if (file != doc.uri)
                        continue;
                }

                if (oZhInfo.parent && (oZhInfo.parent.kind.startsWith("function") || oZhInfo.parent.kind.startsWith("procedure") || oZhInfo.parent.kind == 'method')) {
                    if (file != doc.uri) continue;
                    if (param.position.line < oZhInfo.parent.startLine ||
                        param.position.line > oZhInfo.parent.endLine)
                        continue;
                }
                var added = check_add(oZhInfo.name, get_completition_item_kind_type(oZhInfo.kind, false), "AAA");
                if (added && (oZhInfo.kind == "method" || oZhInfo.kind == "data") && oZhInfo.parent)
                    added.documentation = oZhInfo.parent.name;

                if (cancelled.isCancellationRequested)
                    return;
            }
        }

        for (var file in oFiles) {
            get_completitions(oFiles[file], file);
            if (cancelled.isCancellationRequested)
                return server.CompletionList.create(aCompletitions, false);
        }

        if (provider) {
            get_completitions(provider, doc.uri);
        } else {
            if (doc.uri in oFiles) {
                provider = oFiles[doc.uri];
            }
        }

        if (provider) {
            var thisDone = doc.uri in oFiles;
            var aIncludes = provider.aIncludes;
            var i = 0;
            var startDir = path.dirname(Uri.parse(doc.uri).fsPath);
            while (i < aIncludes.length) {
                var providerInc = zh_parse_include(startDir, aIncludes[i], thisDone);
                if (providerInc) {
                    get_completitions(providerInc, providerInc.currentDocument)
                    for (var j = 0; j < providerInc.aIncludes; j++) {
                        if (aIncludes.indexOf(providerInc.aIncludes[j]) < 0)
                            aIncludes.push(providerInc.aIncludes[j]);
                    }
                }
                i++;
                if (cancelled.isCancellationRequested)
                    return server.CompletionList.create(aCompletitions, false);
            }
        }
        if (precLetter != ':' && precLetter != '->') {
            for (var i = 0; i < aFunctions.length; i++) {
                var oCompletionItem = check_add(aFunctions[i].name, server.CompletionItemKind.Function, "AA")
                if (oCompletionItem) oCompletionItem.documentation = aFunctions[i].documentation;
                if (cancelled.isCancellationRequested)
                    return server.CompletionList.create(aCompletitions, false);
            }
            for (var i = 1; i < aKeywords.length; i++) {
                check_add(aKeywords[i], server.CompletionItemKind.Keyword, "AAA")
                if (cancelled.isCancellationRequested)
                    return server.CompletionList.create(aCompletitions, false);
            }
            for (var i = 1; i < aFunctionsMissing.length; i++) {
                check_add(aFunctionsMissing[i], server.CompletionItemKind.Function, "A")
                if (cancelled.isCancellationRequested)
                    return server.CompletionList.create(aCompletitions, false);
            }
        }

        if (wordBasedSuggestions) {
            var wordRE = /\b[a-z_][a-z0-9_]*\b/gi
            var foundWord;
            var pos = doc.offsetAt(param.position);
            while (foundWord = wordRE.exec(allText)) {
                // remove current word
                if (foundWord.index < pos && foundWord.index + foundWord[0].length >= pos)
                    continue;
                check_add(foundWord[0], server.CompletionItemKind.Text, "");
                if (cancelled.isCancellationRequested)
                    return server.CompletionList.create(aCompletitions, false);
            }
        }
        return server.CompletionList.create(aCompletitions, false);
    })

/**
 *
 * @param {string} word
 * @param {string} startPath
 * @param {server.Range} includeRange
 */
function completition_files(word, startPath, includeRange) {

    var completitons = [];
    word = word.replace("\r", "").replace("\n", "");
    if (process.platform.startsWith("win"))
        word = word.toLowerCase();
    var startDone = false;
    if (startPath) startPath = startPath.toLowerCase();
    var deltaPath = ""
    var lastSlash = Math.max(word.lastIndexOf("\\"), word.lastIndexOf("/"))
    if (lastSlash > 0) {
        deltaPath = word.substr(0, lastSlash);
        word = word.substr(lastSlash + 1);
    }
    var dirDone = [];

    function check_dir(dir) {
        if (startPath && !path.isAbsolute(dir))
            dir = path.join(startPath, dir);
        dir = path.join(dir, deltaPath);
        if (process.platform.startsWith("win")) {
            if (dirDone.indexOf(dir.toLowerCase()) >= 0)
                return;
            dirDone.push(dir.toLowerCase());
        } else {
            if (dirDone.indexOf(dir) >= 0)
                return;
            dirDone.push(dir);
        }
        if (!fs.existsSync(dir))
            return;

        if (startPath && dir.toLowerCase() == startPath)
            startDone = true;

        var aFiles = fs.readdirSync(dir)

        /** @type {Array<String>} */
        var subfiles;
        var extRE = /\.c?h$/i;
        for (var fileIndex = 0; fileIndex < aFiles.length; fileIndex++) {
            var fileName = aFiles[fileIndex];
            if (process.platform.startsWith("win"))
                fileName = fileName.toLowerCase();
            var completePath = path.join(dir, aFiles[fileIndex]);
            var info = fs.statSync(completePath);
            if (info.isDirectory()) {
                subfiles = fs.readdirSync(completePath);
                if (subfiles.findIndex((v) => extRE.test(v)) == -1)
                    continue;
            } else if (!extRE.test(aFiles[fileIndex]))
                continue;
            var sortText = undefined;
            if (word.length != 0) {
                sortText = is_inside(word, fileName);
                if (!sortText)
                    continue;
            }
            var c = server.CompletionItem.create(path.join(deltaPath, aFiles[fileIndex]));
            c.kind = info.isDirectory() ? server.CompletionItemKind.Folder : server.CompletionItemKind.File;
            c.sortText = sortText ? sortText : aFiles[fileIndex];
            c.detail = dir;
            c.textEdit = server.TextEdit.replace(includeRange, path.join(deltaPath, aFiles[fileIndex]).replace("\\", "/"));
            completitons.push(c);
        }
    }

    for (var i = 0; i < workspaceRoots.length; i++) {
        // other scheme of uri unsupported
        /** @type {vscode-uri.default} */
        var uri = Uri.parse(workspaceRoots[i]);
        if (uri.scheme != "file") continue;
        check_dir(uri.fsPath);
    }
    for (var i = 0; i < aIncludeDirs.length; i++) {
        check_dir(aIncludeDirs[i]);
    }
    if (startPath && !startDone) {
        check_dir(startPath);
    }
    return server.CompletionList.create(completitons, false);
}

function definition_files(fileName, startPath, origin) {

    var dest = [];
    fileName = fileName.toLowerCase();
    var startDone = false;

    if (startPath)
        startPath = startPath.toLowerCase();

    function def_dir(dir) {
        if (startPath && !path.isAbsolute(dir))
            dir = path.join(startPath, dir);
        if (!fs.existsSync(dir)) return;
        if (startPath && dir.toLowerCase() == startPath) startDone = true;
        var file = fs.readdirSync(dir)
        for (var fi = 0; fi < file.length; fi++) {
            if (file[fi].toLowerCase() == fileName) {
                var fileUri = Uri.file(path.join(dir, file[fi])).toString();
                if (canLocationLink)
                    dest.push(server.LocationLink.create(fileUri, server.Range.create(0, 0, 0, 0), server.Range.create(0, 0, 0, 0), origin));
                else
                    dest.push(server.Location.create(fileUri, server.Range.create(0, 0, 0, 0)));
            }
        }
    }
    for (var i = 0; i < workspaceRoots.length; i++) {
        // other scheme of uri unsupported
        /** @type {vscode-uri.default} */
        var uri = Uri.parse(workspaceRoots[i]);
        if (uri.scheme != "file")
            continue;
        def_dir(uri.fsPath);
    }
    for (var i = 0; i < aIncludeDirs.length; i++) {
        def_dir(aIncludeDirs[i]);
    }
    if (startPath && !startDone) {
        def_dir(startPath);
    }
    return dest;
}

function completition_dbf_fields(word, allText, pos, provider) {

    //precLetter = '->';
    var pdb = pos - 2;
    var dbfName = "";
    var nBracket = 0;

    while ((allText[pdb] != ' ' && allText[pdb] != '\t') || nBracket > 0) {
        var c = allText[pdb];
        pdb--;
        if (c == ')') nBracket++;
        if (c == '(') nBracket--;
        dbfName = c + dbfName;
    }
    var aCompletitions = [];

    function add_dbf(dbf) {
        for (var f in dbf.fields) {
            var name = dbf.fields[f];
            if (typeof (name) != "string")
                name = name.name;
            var sortText = name;
            if (word.length > 0) {
                sortText = is_inside(word, f);
            }
            if (!sortText)
                continue;
            if (!aCompletitions.find((v) => v.label.toLowerCase() == name.toLowerCase())) {
                var c = server.CompletionItem.create(name);
                c.kind = server.CompletionItemKind.Field;
                c.documentation = dbf.name;
                c.sortText = "AAAA" + sortText;
                aCompletitions.push(c);
            }
        }
    }

    function check_dbf(aDbfs) {

        if (!(dbfName in aDbfs)) {
            // check if pick too much text
            for (var dbf in aDbfs) {
                if (dbfName.endsWith(dbf)) {
                    dbfName = dbf;
                    break;
                }
            }
        }
        if (dbfName in aDbfs) {
            add_dbf(aDbfs[dbfName]);
        }
    }

    dbfName = dbfName.toLowerCase().
        replace(" ", "").
        replace("\t", "");

    if (dbfName.toLowerCase() == "field") {
        for (var dbf in oDbfs)
            add_dbf(oDbfs[dbf]);
        if (provider) {
            for (var db in provider.aDbfs)
                add_dbf(provider.aDbfs[dbf]);
        }
    } else {
        check_dbf(oDbfs);
        if (provider && dbfName in provider.aDbfs) {
            check_dbf(provider.aDbfs);
        }
    }
    return aCompletitions;
}

connection.onHover(
    (params, cancelled) => {
        var w = get_word(params);
        var doc = documents.get(params.textDocument.uri);
        var provider = get_document_provider(doc);
        if (provider) {
            for (var iSign = 0; iSign < provider.aFunctions.length; iSign++) {
                var info = provider.aFunctions[iSign];
                if (info.kind != 'define')
                    continue;
                if (info.name != w)
                    continue;
                return {
                    contents: {
                        language: 'ziher',
                        value: info.body
                    }
                };
            }

            var thisDone = doc.uri in oFiles;
            var aIncludes = provider.aIncludes;
            var i = 0;
            var startDir = path.dirname(Uri.parse(doc.uri).fsPath);
            while (i < aIncludes.length) {
                var providerInc = zh_parse_include(startDir, aIncludes[i], thisDone);
                if (providerInc) {
                    for (var iSign = 0; iSign < providerInc.aFunctions.length; iSign++) {
                        var info = providerInc.aFunctions[iSign];
                        if (info.kind != 'define') continue;
                        if (info.name != w) continue
                        return { contents: { language: 'ziher', value: info.body } };
                    }
                    for (var j = 0; j < providerInc.aIncludes; j++) {
                        if (aIncludes.indexOf(providerInc.aIncludes[j]) < 0)
                            aIncludes.push(providerInc.aIncludes[j]);
                    }
                }
                i++;
                if (cancelled.isCancellationRequested)
                    return server.CompletionList.create(completitions, false);
            }
        }
        return undefined;
    })

connection.onFoldingRanges(
    (params) => {
        var ranges = [];
        var doc = documents.get(params.textDocument.uri);
        var provider = get_document_provider(doc, true);
        for (var iSign = 0; iSign < provider.aFunctions.length; iSign++) {
            /** @type {provider.ZhInfo} */
            var info = provider.aFunctions[iSign];
            if (info.startLine != info.endLine) {
                var oRange = {};
                oRange.startLine = info.startLine;
                oRange.endLine = info.endLine;
                ranges.push(oRange);
            }
        }
        var deltaLine = 0;
        if (lineFoldingOnly) deltaLine = 1;
        for (let iGroup = 0; iGroup < provider.aGroups.length; iGroup++) {
            /** @type {provider.KeywordPos[]} */
            var aKeywordPositions = provider.aGroups[iGroup].aKeywordPositions;
            if (["if", "try", "sequence", "case"].indexOf(provider.aGroups[iGroup].type) < 0) {
                var oRange = {};
                var i = aKeywordPositions.length - 1;
                oRange.startLine = aKeywordPositions[0].line;
                oRange.endLine = aKeywordPositions[i].line - deltaLine;
                oRange.startCharacter = aKeywordPositions[0].endCol;
                oRange.endCharacter = aKeywordPositions[i].startCol;
                ranges.push(oRange);
            } else {
                var prec = 0;
                for (let i = 1; i < aKeywordPositions.length; i++) {
                    if (aKeywordPositions[i].text != "exit") {
                        var oRange = {};
                        oRange.startLine = aKeywordPositions[prec].line;
                        oRange.endLine = aKeywordPositions[i].line - deltaLine;
                        oRange.startCharacter = aKeywordPositions[prec].endCol;
                        oRange.endCharacter = aKeywordPositions[i].startCol;
                        ranges.push(oRange);
                        prec = i;
                    }
                }
            }
        }
        for (var iGroup = 0; iGroup < provider.aPreprocGroups.length; iGroup++) {
            /** @type {provider.KeywordPos[]} */
            var aKeywordPositions = provider.aPreprocGroups[iGroup].aKeywordPositions;
            var oRange = {};
            var i = aKeywordPositions.length - 1;
            oRange.startLine = aKeywordPositions[0].line;
            oRange.endLine = aKeywordPositions[i].line - deltaLine;
            oRange.startCharacter = aKeywordPositions[0].endCol;
            oRange.endCharacter = aKeywordPositions[i].startCol;
            ranges.push(oRange);
        }
        for (let iComment = 0; iComment < provider.multilineComments.length; iComment++) {
            const cc = provider.multilineComments[iComment];
            var oRange = {};
            oRange.king = "comment"
            oRange.startLine = cc[0];
            oRange.endLine = cc[1];
            ranges.push(oRange);
        }
        for (let iCFolder = 0; iCFolder < provider.cCodeFolder.length; iCFolder++) {
            const folder = provider.cCodeFolder[iCFolder];
            var oRange = {};
            oRange.startLine = folder[0]
            oRange.endLine = folder[2] - deltaLine;
            oRange.startCharacter = folder[1];
            oRange.endCharacter = folder[3];
            ranges.push(oRange);

        }

        return ranges;
    })

connection.onRequest("groupAtPosition",
    (params) => {
        var doc = documents.get(params.textDocument.uri);
        var provider = get_document_provider(doc, true);
        for (var iGroup = 0; iGroup < provider.aGroups.length; iGroup++) {
            /** @type {Array<provider.KeywordPos>} */
            var poss = provider.aGroups[iGroup].aKeywordPositions;
            for (var i = 0; i < poss.length; i++) {
                if (params.sel.active.line == poss[i].line &&
                    params.sel.active.character >= poss[i].startCol &&
                    params.sel.active.character <= poss[i].endCol) {
                    return poss;
                }
            }
        }
        return [];
    })

connection.onRequest("docSnippet",
    (params) => {
        var doc = documents.get(params.textDocument.uri);
        var provider = get_document_provider(doc);

        /** @type{provider.ZhInfo} */
        var funcInfo, iSign;
        for (let i = 0; i < provider.aFunctions.length; i++) {
            /** @type{provider.ZhInfo} */
            const info = provider.aFunctions[i];
            if (!info.kind.startsWith("procedure") &&
                !info.kind.startsWith("function"))
                continue;

            if (info.startLine > params.sel[0].line) {
                funcInfo = info;
                iSign = i;
                break;
            }
        }
        if (!funcInfo)
            return undefined;

        if ("zhDocIdx" in funcInfo)
            return undefined;

        var subParams = [];
        for (var iParam = iSign + 1; iParam < provider.aFunctions.length; iParam++) {
            /** @type {provider.ZhInfo} */
            var subinfo = provider.aFunctions[iParam];
            if (subinfo.parent == funcInfo && subinfo.kind == "param") {
                subParams.push(subinfo);
            } else
                break;
        }

        var snipppet = "/* \\$DOC\\$\r\n";
        snipppet += "\t\\$TEMPLATE\\$\r\n\t\t" + funcInfo.kind + "\r\n";
        snipppet += "\t\\$ONELINER\\$\r\n\t\t$1\r\n";
        snipppet += "\t\\$SYNTAX\\$\r\n\t\t" + funcInfo.name + "(";
        for (let iParam = 0; iParam < subParams.length; iParam++) {
            const param = subParams[iParam];
            snipppet += "<" + param.name + ">";
            if (iParam != subParams.length - 1)
                snipppet += ", ";
        }
        if (funcInfo.kind.startsWith("function"))
            snipppet += ") --> ${2:retValue}\r\n";
        else
            snipppet += ")\r\n";
        snipppet += "\t\\$ARGUMENTS\\$\r\n";
        var nTab = 3;
        for (let iParam = 0; iParam < subParams.length; iParam++) {
            const param = subParams[iParam];
            snipppet += "\t\t<" + param.name + "> $" + nTab + "\r\n";
            nTab++;
        }
        if (funcInfo.kind.startsWith("function")) {
            snipppet += "\t\\$RETURNS\\$\r\n"
            snipppet += "\t\t${2:retValue} $" + nTab + "\r\n"
        }
        snipppet += "\t\\$END\\$ */"
        return snipppet;
    })

//connection.onDocumentFormatting =

connection.listen();
