import {
    Logger, logger,
    LoggingDebugSession,
    InitializedEvent, TerminatedEvent, StoppedEvent, BreakpointEvent, OutputEvent,
    Thread, StackFrame, Scope, Source, Handles, Breakpoint,
    Variable, ContinuedEvent, CompletionItem
} from 'vscode-debugadapter';

import { DebugProtocol } from 'vscode-debugprotocol';
//import { basename } from 'path';
//import { MockRuntime, MockBreakpoint } from './mockRuntime';

//const { Subject } = require('await-notify');
const { process } = require("process");
const { path } = require("path");
const { fs } = require("fs");
const { cp } = require("child_process");
//const { localize } = require("./localize.js").localize;
import * as net from "net";

function timeout(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}


/** https://stackoverflow.com/questions/33086985/how-to-obtain-case-exact-path-of-a-file-in-node-js-on-windows
 * @param {string} filePath
 * @returns {string|undefined}
 */
function zh_getRealPath(filePath: string) {

    console.log(`getRealPath ${filePath}`);
    if (!process.platform.startsWith("win"))
        return filePath;

    let i: number;

    let dirname = path.dirname(filePath);
    let lowerFileName = path.basename(filePath).toLowerCase();
    let fileNames: string[] = fs.readdirSync(dirname);

    for (i = 0; i < fileNames.length; i += 1) {
        if (fileNames[i].toLowerCase() === lowerFileName) {
            return path.join(dirname, fileNames[i]);
        }
    }
}

/**
 * This interface describes the mock-debug specific launch attributes
 * (which are not part of the Debug Adapter Protocol).
 * The schema for these attributes lives in the package.json of the mock-debug extension.
 * The interface should always match this schema.
 */
interface LaunchRequestArguments extends DebugProtocol.LaunchRequestArguments {

    /** An absolute path to the "program" to debug. */
    program: string;

    /** Automatically stop target after launch. If not specified, target does not stop. */
    stopOnEntry?: boolean;

    /** enable logging the Debug Adapter Protocol */
    trace?: boolean;

    port: number;
    terminalType: string;
    workingDir: string;
    arguments: any;
    workspaceRoot: string;
    sourcePaths: any;

}


export class ZhDebugSession extends LoggingDebugSession {

    private socket: any;

    //private Debbugging: boolean = true;

    private sourcePaths: string[] = [];

    private processLine = undefined;

    private breakpoints = {};

    private variableCommands: string[] = [];

    private variableEvaluations: string[] = [];

    private stack: DebugProtocol.StackTraceResponse[] = [];
    private stackArgs: DebugProtocol.StackTraceArguments[] = [];

    private justStart = true;

    private queue: string = "";

    private evaluateResponses: any[] = [];

    private completionsResponse: DebugProtocol.CompletionsResponse;

    private processId: number;

    private currentStack: number;

    private scopeResponse: DebugProtocol.ScopesResponse;

    private varResp: any[] = [];

    private startGo: boolean;
    private Debugging: boolean;


    // we don't support multiple threads, so we can use a hardcoded ID for the default thread
    //private static THREAD_ID = 1;

    // a Mock runtime (or debugger)
    //private _runtime: MockRuntime;

    //private _variableHandles = new Handles<string>();

    //private _configurationDone = new Subject();

    //private _cancelationTokens = new Map<number, boolean>();
    //private _isLongrunning = new Map<number, boolean>();

	/**
	 * Creates a new debug adapter that is used for one debug session.
	 * We configure the default implementation of a debug adapter here.
	 */

    public constructor() {
        //super("mock-debug.txt");
        super();

        // this debugger uses zero-based lines and columns
        this.setDebuggerLinesStartAt1(false);
        this.setDebuggerColumnsStartAt1(false);

        /*
		this._runtime = new MockRuntime();

		// setup event handlers
		this._runtime.on('stopOnEntry', () => {
			this.sendEvent(new StoppedEvent('entry', MockDebugSession.THREAD_ID));
		});
		this._runtime.on('stopOnStep', () => {
			this.sendEvent(new StoppedEvent('step', MockDebugSession.THREAD_ID));
		});
		this._runtime.on('stopOnBreakpoint', () => {
			this.sendEvent(new StoppedEvent('breakpoint', MockDebugSession.THREAD_ID));
		});
		this._runtime.on('stopOnDataBreakpoint', () => {
			this.sendEvent(new StoppedEvent('data breakpoint', MockDebugSession.THREAD_ID));
		});
		this._runtime.on('stopOnException', () => {
			this.sendEvent(new StoppedEvent('exception', MockDebugSession.THREAD_ID));
		});
		this._runtime.on('breakpointValidated', (bp: MockBreakpoint) => {
			this.sendEvent(new BreakpointEvent('changed', <DebugProtocol.Breakpoint>{ verified: bp.verified, id: bp.id }));
		});
		this._runtime.on('output', (text, filePath, line, column) => {
			const e: DebugProtocol.OutputEvent = new OutputEvent(`${text}\n`);
			e.body.source = this.createSource(filePath);
			e.body.line = this.convertDebuggerLineToClient(line);
			e.body.column = this.convertDebuggerColumnToClient(column);
			this.sendEvent(e);
		});
		this._runtime.on('end', () => {
			this.sendEvent(new TerminatedEvent());
        });
        */
    }


    private zh_command(cmd: string) {

        if (this.justStart)
            this.queue += cmd;
        else
            this.socket.write(cmd);
    }

    /**
	 * The 'initialize' request is the first request called by the frontend
	 * to interrogate the features the debug adapter provides.
	 */
    protected initializeRequest(response: DebugProtocol.InitializeResponse, args: DebugProtocol.InitializeRequestArguments): void {

        /*
        // build and return the capabilities of this debug adapter:
        response.body = response.body || {};

        // the adapter implements the configurationDoneRequest.
        response.body.supportsConfigurationDoneRequest = true;

        // make VS Code to use 'evaluate' when hovering over source
        response.body.supportsEvaluateForHovers = true;

        // make VS Code to show a 'step back' button
        response.body.supportsStepBack = true;

        // make VS Code to support data breakpoints
        response.body.supportsDataBreakpoints = true;

        // make VS Code to support completion in REPL
        response.body.supportsCompletionsRequest = true;
        response.body.completionTriggerCharacters = [".", "["];

        // make VS Code to send cancelRequests
        response.body.supportsCancelRequest = true;

        // make VS Code send the breakpointLocations request
        response.body.supportsBreakpointLocationsRequest = true;

        this.sendResponse(response);

        // since this debug adapter can accept configuration requests like 'setBreakpoint' at any time,
        // we request them early by sending an 'initializeRequest' to the frontend.
        // The frontend will end the configuration sequence by calling 'configurationDone' request.
        this.sendEvent(new InitializedEvent());
        */

        //if (args.locale) {
            // require("./localize.js").reInit(args);
        //}

        response.body = response.body || {};
        response.body.supportsConfigurationDoneRequest = true;
        response.body.supportsDelayedStackTraceLoading = false;
        response.body.supportsConditionalBreakpoints = true;
        response.body.supportsHitConditionalBreakpoints = true;
        //response.body.supportsLogPoint = true;
        response.body.supportsCompletionsRequest = true;
        response.body.supportsTerminateRequest = true;
        response.body.exceptionBreakpointFilters = [
            {
                //label: localize('ziher.dbgError.all'),
                label: "Stop on all errors",
                filter: 'all',
                default: false
            },
            {
                //label: localize('ziher.dbgError.notSeq'),
                label: "Stop only on out-of-sequence errors",
                filter: 'notSeq',
                default: true
            }
        ];
        //response.body.supportsEvaluateForHovers = true; too risky
        this.sendResponse(response);
    }

    protected launchRequest(response: DebugProtocol.LaunchResponse, args: LaunchRequestArguments, request?: DebugProtocol.Request) {


		logger.setup(args.trace ? Logger.LogLevel.Verbose : Logger.LogLevel.Stop, false);

        var port = args.port ? args.port : 6110;
        console.log(`launchRequest - starts the server listen ${port}`);

        //var tc = this;
        this.justStart = true;
        this.sourcePaths = []; //[path.dirname(args.program)];

        if ("workspaceRoot" in args) {
            this.sourcePaths.push(args.workspaceRoot);
        }
        if ("sourcePaths" in args) {
            this.sourcePaths = this.sourcePaths.concat(args.sourcePaths);
        }

        this.Debugging = !args.noDebug;

        this.startGo = args.stopOnEntry === false || args.noDebug === true;

        // https://stackoverflow.com/questions/30545245/beginner-node-js-net-createserver-example

        let server = net.createServer((socket: any) => {
            this.zh_evaluateClient(socket, server, args)
        }).listen(port);

        let cbRunInTerminal = (response: DebugProtocol.RunInTerminalResponse) => {

        };


        // starts the program
        //console.log("start the program");
        switch (args.terminalType) {

            case 'external':
            case 'integrated':
                this.runInTerminalRequest({
                    "kind": args.terminalType,
                    "cwd": args.workingDir,
                    "args": [args.program].concat(args.arguments ? args.arguments : [])
                }, 10000, cbRunInTerminal);
                break;

            case 'none':
            default:
                var process;
                if (args.arguments)
                    process = cp.spawn(args.program, args.arguments, { cwd: args.workingDir });
                else
                    process = cp.spawn(args.program, { cwd: args.workingDir });

                process.on("error", e => {
                    //this.sendEvent(new OutputEvent(localize("ziher.dbgError1", args.program, args.workingDir), "stderr"))
                    this.sendEvent(new OutputEvent( `Unable to start ${args.program} in ${args.workingDir}, check that all path exists`, "stderr"));                   
                    this.sendEvent(new TerminatedEvent());
                    return;
                });

                process.stderr.on('data', data =>
                    this.sendEvent(new OutputEvent(data.toString(), "stderr"))
                );

                process.stdout.on('data', data =>
                    this.sendEvent(new OutputEvent(data.toString(), "stdout"))
                );
                break;
        }
        this.sendResponse(response);
    }

    

    private zh_evaluateClient(socket: any, server, args) {

        var nData = 0;
        //var tc = this;
        var exeTarget = path.basename(args.program, path.extname(args.program)).toLowerCase();

        socket.on("data", (data) => {

            if (this.socket && nData > 0) {
                this.zh_processInput(data.toString())
                return;
            }
            if (nData > 0) {
                return
            }
            nData++;

            // the client sent exe name and process ID		
            var lines = data.toString().split("\r\n");
            if (lines.length < 2) //todo: check if they arrive in 2 tranches.
                return;

            var clPath = path.basename(lines[0], path.extname(lines[0])).toLowerCase();
            if (clPath != exeTarget) {
                socket.write("NO\r\n")
                socket.end();
                return;
            }
            socket.write("HELLO\r\n")
            this.zh_setProcess(parseInt(lines[1]));

            //connected!
            this.sendEvent(new InitializedEvent());
            server.close();
            this.socket = socket;

            socket.removeAllListeners("data");
            socket.on("data", data => {
                this.zh_processInput(data.toString())
            });
            socket.write(this.queue);
            this.justStart = false;
            this.queue = "";
        });
    }


    protected configurationDoneRequest(response: DebugProtocol.ConfigurationDoneResponse, args: DebugProtocol.ConfigurationDoneArguments, request?: DebugProtocol.Request) {

        if (this.startGo) {
            this.zh_command("GO\r\n");
            this.sendEvent(new ContinuedEvent(1, true));
        }
        this.sendResponse(response);
    }


    // line type ?
    private zh_sendVariables(id, line: any) {

        var vars = [];
        this.processLine = function (line: any) {

            if (line.startsWith("END")) {
                this.varResp[id].body = {
                    variables: vars
                };
                this.sendResponse(this.varResp[id]);
                this.processLine = undefined;
                return;
            }

            var infos = line.split(":");
            line = infos[0] + ":" + infos[1] + ":" + infos[2] + ":" + infos[3];
            if (infos.length > 7) { //the value can contains : , we need to rejoin it.
                infos[6] = infos.splice(6).join(":");
            }
            var v = new Variable(infos[4], infos[6], line);
            v = this.zh_getVariableFormat(v, infos[5], infos[6], "value", line, id);
            vars.push(v);
        }
    }

    private zh_sendScope(inError: boolean) {

        // reset references
        this.variableCommands = [];
        if (inError)
            this.variableCommands.push("ERROR_VAR");

        this.variableCommands = this.variableCommands.concat([
            "LOCALS",
            "PUBLICS",
            "PRIVATES",
            "PRIVATE_CALLEE",
            "STATICS"
        ]);

        //TODO: "GLOBALS","EXTERNALS"
        this.varResp = [];
        this.varResp.length = this.variableCommands.length;
        this.variableEvaluations = [];
        this.variableEvaluations.length = this.variableCommands.length;
        var n = 0;
        var scopes = [];
        if (inError) {
            n = 1;
            scopes.push(new Scope("Error", 1))
        }
        scopes = scopes.concat([
            new Scope("Local", n + 1),
            new Scope("Public", n + 2),
            new Scope("Private local", n + 3),
            new Scope("Private external", n + 4),
            new Scope("Statics", n + 5)
            //new Scope("Gobals",6),
            //new Scope("Externals",7)
        ]);
        var response = this.scopeResponse;
        response.body = { scopes: scopes };
        this.sendResponse(response)
    }


    private zh_getVarReference(line: string, evaluate: any) {

        var r = this.variableCommands.indexOf(line);
        if (r >= 0)
            return r + 1;

        var infos = line.split(":");

        //the value can contains : , we need to rejoin it.
        if (infos.length > 4) {
            infos[2] = infos.splice(2).join(":").slice(0, -1);
            infos.length = 3;
            line = infos.join(":") + ":";
        }

        this.variableCommands.push(line);
        this.variableEvaluations.push(evaluate);
        this.sendEvent(new OutputEvent("added variable command:'" + line + "'\r\n", "stdout"))
        return this.variableCommands.length;
    }


    private zh_getVariableFormat(dest, type, value, valueName, line, id?: any): any {

        if (type == "C") {
            value = value.replace(/\\\$\\n/g, "\n");
            value = value.replace(/\\\$\\r/g, "\r");
        }
        dest[valueName] = value;
        dest.type = type;

        if (["E", "B", "P"].indexOf(dest.type) == -1) {
            dest.evaluateName = "";
            if (this.variableEvaluations[id])
                dest.evaluateName = this.variableEvaluations[id];
            dest.evaluateName += dest.name;
            if (this.variableEvaluations[id] && this.variableEvaluations[id].endsWith("["))
                dest.evaluateName += "]";
        }
        switch (type) {

            case "A":
                dest.variablesReference = this.zh_getVarReference(line, dest.evaluateName + "[");
                dest[valueName] = `ARRAY(${value})`;
                dest.indexedVariables = parseInt(value);
                break;

            case "H":
                dest.variablesReference = this.zh_getVarReference(line, dest.evaluateName + "[");
                dest[valueName] = `HASH(${value})`;
                dest.namedVariables = parseInt(value);
                break;

            case "O":
                dest.variablesReference = this.zh_getVarReference(line, dest.evaluateName + ":");
                var infos = value.split(" ");
                dest[valueName] = `CLASS ${infos[0]}`;
                dest.namedVariables = parseInt(infos[1]);
                break;
        }
        return dest;
    }

    /**
     * Evaluate the return from an expression request
     * line the income line
     */
    private zh_processExpression(line: string) {

        // EXPRESSION:{frame}:{type}:{result}
        var infos = line.split(":");
        //the value can contains : , we need to rejoin it.
        if (infos.length > 4) {
            infos[3] = infos.splice(3).join(":");
        }
        var resp = this.evaluateResponses.shift();
        var line = "EXP:" + infos[1] + ":" + resp.body.result.replace(/:/g, ";") + ":";
        resp.body.name = resp.body.result
        if (infos[2] == "E") {
            resp.success = false;
            resp.message = infos[3];
        } else
            resp.body = this.zh_getVariableFormat(resp.body, infos[2], infos[3], "result", line);
        this.sendResponse(resp);
    }


    private zh_sendStack(line: string) {

        var nStack = parseInt(line.substring(6));
        var frames = [];
        frames.length = nStack;
        var j = 0;
        this.processLine = function (line) {
            var infos = line.split(":");
            for (var i = 0; i < infos.length; i++) infos[i] = infos[i].replace(";", ":")
            var completePath = infos[0];
            console.log(`completePath=${completePath}`);
            var found = false;
            if (path.isAbsolute(infos[0]) && fs.existsSync(infos[0])) {
                completePath = zh_getRealPath(infos[0]);
                found = true;
            } else
                for (let i = 0; i < this.sourcePaths.length; i++) {
                    if (fs.existsSync(path.join(this.sourcePaths[i], infos[0]))) {
                        completePath = zh_getRealPath(path.join(this.sourcePaths[i], infos[0]));
                        found = true;
                        break;
                    }
                }
            if (found) infos[0] = path.basename(completePath);
            frames[j] = new StackFrame(j, infos[2],
                new Source(infos[0], completePath),
                parseInt(infos[1]));
            j++;
            if (j == nStack) {
                while (this.stack.length > 0) {
                    var args = this.stackArgs.shift();
                    var resp = this.stack.shift();
                    args.startFrame = args.startFrame || 0;
                    args.levels = args.levels || frames.length;
                    args.levels += args.startFrame;
                    resp.body = {
                        stackFrames: frames.slice(args.startFrame, args.levels)
                    };
                    this.sendResponse(resp);
                }
                this.processLine = undefined;
            }
        }
    }

    private zh_processBreak(line: string) {

        this.sendEvent(new OutputEvent("received: " + line + "\r\n", "console"));

        // ? aInfos type ??
        var aInfos: any[] = line.split(":");
        var dest;

        if (!(aInfos[1] in this.breakpoints)) {
            //error 
            return;
        }
        aInfos[2] = parseInt(aInfos[2]);
        aInfos[3] = parseInt(aInfos[3]);
        dest = this.breakpoints[aInfos[1]];

        var idBreak = dest.response.body.breakpoints.findIndex(b => b.line == aInfos[2]);
        if (idBreak == -1) {
            if (aInfos[2] in dest) {
                delete dest[aInfos[2]];
                this.zh_checkBreakPoint(aInfos[1]);
            }
            return;
        }
        if (aInfos[3] > 1) {
            dest.response.body.breakpoints[idBreak].line = aInfos[3];
            dest.response.body.breakpoints[idBreak].verified = true;
            dest[aInfos[2]] = 1;
        } else {
            dest.response.body.breakpoints[idBreak].verified = false;
            if (aInfos[4] == 'notfound')
                //dest.response.body.breakpoints[idBreak].message = localize('ziher.dbgNoModule')
                dest.response.body.breakpoints[idBreak].message = "Module not found";
            else
               // dest.response.body.breakpoints[idBreak].message = localize('ziher.dbgNoLine')
               dest.response.body.breakpoints[idBreak].message = "Invalid line";

            dest[aInfos[2]] = 1;
        }
        this.zh_checkBreakPoint(aInfos[1]);
    }

    private zh_checkBreakPoint(src) {

        var dest = this.breakpoints[src];
        for (var i in dest) {
            if (dest.hasOwnProperty(i) && i != "response") {
                if (dest[i] != 1) {
                    return;
                }
            }
        }
        //this.sendEvent(new debugadapter.OutputEvent("Response "+src+"\r\n","console"))
        this.sendResponse(dest.response);
    }


    private zh_processInput(buff: string) {

        var lines = buff.split("\r\n");
        for (var i = 0; i < lines.length; i++) {
            var line = lines[i];
            //if (!line.startsWith("LOG:")) this.sendEvent(new debugadapter.OutputEvent(">>"+line+"\r\n","stdout"))
            if (line.length == 0)
                continue;

            if (this.processLine) {
                this.processLine(line);
                continue;
            }
            if (line.startsWith("STOP")) {
                this.sendEvent(new StoppedEvent(line.substring(5), 1));
                continue;
            }
            if (line.startsWith("STACK")) {
                this.zh_sendStack(line);
                continue;
            }
            if (line.startsWith("BREAK")) {
                this.zh_processBreak(line);
                continue;
            }
            if (line.startsWith("ERROR") && !line.startsWith("ERROR_VAR")) {
                //console.log("ERROR")
                var stopEvt = new StoppedEvent("error", 1, line.substring(6));
                this.sendEvent(stopEvt);
                continue;
            }
            if (line.startsWith("EXPRESSION")) {
                this.zh_processExpression(line);
                continue;
            }
            if (line.startsWith("LOG")) {
                this.sendEvent(new OutputEvent(line.substring(4) + "\r\n", "stdout"))
                continue;
            }
            if (line.startsWith("INERROR")) {
                this.zh_sendScope(line[8] == 'T')
                continue;
            }
            if (line.startsWith("COMPLETITION")) {
                this.zh_processCompletion(line);
                continue;
            }
            for (var j = this.variableCommands.length - 1; j >= 0; j--) {
                if (line.startsWith(this.variableCommands[j])) {
                    this.zh_sendVariables(j, line);
                    break;
                }
            }
            if (j != this.variableCommands.length)
                continue;
        }
    }

    private zh_processCompletion(line: any) {

        this.processLine = function (line) {
            if (line == "END") {
                this.sendResponse(this.completionsResponse);
                this.processLine = undefined;
                return;
            }
            if (!this.completionsResponse.body)
                this.completionsResponse.body = {};

            if (!this.completionsResponse.body.targets)
                this.completionsResponse.body.targets = [];

            var type = line.substr(0, line.indexOf(":"));
            line = line.substr(line.indexOf(":") + 1);

            let thisCompletion = new CompletionItem(line, 0);

            // function/procedure -> function
            // method -> field
            // data -> variable
            // local/public/etc -> value

            let label: string;
            if (type == "F")
                label = 'function';
            else if (type == "M")
                label = 'field';
            else if (type == "D")
                label = 'variable';
            else
                label = 'value';

            thisCompletion.label = label;

            this.completionsResponse.body.targets.push(thisCompletion);
        }
    }


    protected setExceptionBreakPointsRequest(response: DebugProtocol.SetExceptionBreakpointsResponse, args: DebugProtocol.SetExceptionBreakpointsArguments, request?: DebugProtocol.Request): void {

        var errorType = args.filters.length;
        // 0 - no stop on error
        // 1 - stop only ut-of-sequence
        // 2 - stop all
        if (errorType == 1 && args.filters[0] != 'notSeq') {
            errorType++;
        }
        this.zh_command(`ERRORTYPE\r\n${errorType}\r\n`)
        this.sendResponse(response);
    }

    protected evaluateRequest(response: DebugProtocol.EvaluateResponse, args: DebugProtocol.EvaluateArguments, request?: DebugProtocol.Request): void {

        //response.body = {};
        response.body.result = args.expression;
        this.evaluateResponses.push(response);
        this.zh_command(`EXPRESSION\r\n${args.frameId + 1 || this.currentStack}:${args.expression.replace(/:/g, ";")}\r\n`)
    }


    protected variablesRequest(response: DebugProtocol.VariablesResponse, args: DebugProtocol.VariablesArguments, request?: DebugProtocol.Request) {

        var zhStart = args.start ? args.start + 1 : 1;
        var zhCount = args.count ? args.count : 0;
        //var prefix;
        if (args.variablesReference <= this.variableCommands.length) {
            this.varResp[args.variablesReference - 1] = response;
            this.zh_command(`${this.variableCommands[args.variablesReference - 1]}\r\n` +
                `${this.currentStack}:${zhStart}:${zhCount}\r\n`);
        } else
            this.sendResponse(response);
    }


    protected scopesRequest(response: DebugProtocol.ScopesResponse, args: DebugProtocol.ScopesArguments, request?: DebugProtocol.Request) {

        // save wanted stack
        this.currentStack = args.frameId + 1;
        this.scopeResponse = response;
        this.zh_command("INERROR\r\n");
    }

    protected completionsRequest(response: DebugProtocol.CompletionsResponse, args: DebugProtocol.CompletionsArguments, request?: DebugProtocol.Request) {

        this.completionsResponse = response;
        this.zh_command(`COMPLETITION\r\n${args.frameId + 1 || this.currentStack}:${args.text}\r\n`)

    }

    protected stackTraceRequest(response: DebugProtocol.StackTraceResponse, args: DebugProtocol.StackTraceArguments): void {

        /*
        const startFrame = typeof args.startFrame === 'number' ? args.startFrame : 0;
        const maxLevels = typeof args.levels === 'number' ? args.levels : 1000;
        const endFrame = startFrame + maxLevels;
    
        const stk = this._runtime.stack(startFrame, endFrame);
    
        response.body = {
            stackFrames: stk.frames.map(f => new StackFrame(f.index, f.name, this.createSource(f.file), this.convertDebuggerLineToClient(f.line))),
            totalFrames: stk.count
        };
        this.sendResponse(response);
        */

        if (this.stack.length == 0)
            this.zh_command("STACK\r\n");

        this.stack.push(response);
        this.stackArgs.push(args);
    }


    private zh_setProcess(pid) {

        var tc = this;
        this.processId = pid;
        var interval = setInterval(() => {
            try {
                process.kill(pid, 0);
            } catch (error) {
                tc.sendEvent(new TerminatedEvent());
                clearInterval(interval);
            }
        }, 1000);

    }

    protected terminateRequest(response: DebugProtocol.TerminateResponse, args: DebugProtocol.TerminateArguments, request?: DebugProtocol.Request) {

        process.kill(this.processId, 'SIGKILL');
        this.sendResponse(response);
    }




}