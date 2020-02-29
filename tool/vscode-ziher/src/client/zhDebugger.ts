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
//const { process } = require("process");
import * as process from "process";

//const { path } = require("path");
import * as path from 'path';

//const { fs } = require("fs");
import * as fs from 'fs';


//const { cp: childProcess } = require("child_process");
import * as childProcess from "child_process";

//const { localize } = require("./localize.js").localize;
import * as net from "net";
const DEBUG_PORT = 60110;

function timeout(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
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
    //workspaceRoot: string;
    sourcePaths: any;

}

interface AttachRequestArguments extends DebugProtocol.AttachRequestArguments {

    port: number;
    noDebug: boolean;

    //terminalType: string;
    workingDir: string;
    //arguments: any;
    //workspaceRoot: string;
    sourcePaths: any;

}

export class ZhDebugSession extends LoggingDebugSession {

    private socket: any;

    private Debugging: boolean;
    private sourcePaths: string[] = [];
    private zhProcessLineCallback = undefined;
    private breakpoints = {};
    private variableReferences: string[] = [];
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
    private tcpServer;

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


    private zh_getRealPath(filePath: string) {

        if (!process.platform.startsWith("win")) {
            this.sendEvent(new OutputEvent(`zh_getRealPath ${process.platform} => [!windows]=${filePath}\n`));
            return filePath;
        }


        let dirname = path.dirname(filePath);
        let lowerFileName = path.basename(filePath).toLowerCase();
        let fileNames: string[] = fs.readdirSync(dirname);

        for (let i = 0; i < fileNames.length; i += 1) {
            if (fileNames[i].toLowerCase() === lowerFileName) {
                this.sendEvent(new OutputEvent(`zh_getRealPath=${path.join(dirname, fileNames[i])}\n`));
                return path.join(dirname, fileNames[i]);
            }
        }
    }

    private zh_command(cmd: string) {

        if (this.justStart) {
            this.sendEvent(new OutputEvent(`zh_command: add to this.queue: ${cmd}\n`));
            this.queue += cmd;
        } else {
            this.sendEvent(new OutputEvent(`zh_command: ${cmd}\n`));  
            this.socket.write(cmd);
        }
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

    protected configurationDoneRequest(response: DebugProtocol.ConfigurationDoneResponse, args: DebugProtocol.ConfigurationDoneArguments, request?: DebugProtocol.Request) {

        if (this.startGo) {
            this.zh_command("GO\r\n");
            this.sendEvent(new ContinuedEvent(1, true));
        }
        this.sendResponse(response);
    }

    protected launchRequest(response: DebugProtocol.LaunchResponse, args: LaunchRequestArguments, request?: DebugProtocol.Request) {


        logger.setup(args.trace ? Logger.LogLevel.Verbose : Logger.LogLevel.Stop, false);
        var port = args.port ? args.port : DEBUG_PORT;

        this.justStart = true;

        this.sendEvent(new OutputEvent(`args=${JSON.stringify(args)}\n`));

        if ("workingDir" in args) {
            this.sourcePaths.push(args.workingDir);
        }
        if ("sourcePaths" in args) {
            this.sourcePaths = this.sourcePaths.concat(args.sourcePaths);
        } else {
            this.sourcePaths = [];
        }

        this.sendEvent(new OutputEvent(`sourcePaths=${JSON.stringify(this.sourcePaths)}\n`));

        this.Debugging = !args.noDebug;
        this.startGo = args.stopOnEntry === false || args.noDebug === true;


        this.sendEvent(new OutputEvent(`RUN tcpServer=${JSON.stringify(this.tcpServer)} ${port}\n`));

        let cbRunInTerminal = (response: DebugProtocol.RunInTerminalResponse) => {

            this.sendEvent(new OutputEvent("RUN IN TERMINAL callback\n"));
            this.tcpServer = net.createServer((socket: any) => {
                this.sendEvent(new OutputEvent(`RUN socket=${JSON.stringify(socket)}\n`));
                this.zh_evaluateClient(socket, this.tcpServer, args);
            }).listen(port, "127.0.0.1");
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
                }, 100000, cbRunInTerminal);
                break;

            case 'none':
            default:
                let child: childProcess.ChildProcess;
                if (args.arguments)
                    child = childProcess.spawn(args.program, args.arguments, { cwd: args.workingDir });
                else
                    child = childProcess.spawn(args.program, { cwd: args.workingDir });

                child.on("error", e => {
                    //this.sendEvent(new OutputEvent(localize("ziher.dbgError1", args.program, args.workingDir), "stderr"))
                    this.sendEvent(new OutputEvent(`Unable to start ${args.program} in ${args.workingDir}, check that all path exists`, "stderr"));
                    this.sendEvent(new TerminatedEvent());
                    return;
                });

                child.stderr.on('data', data =>
                    this.sendEvent(new OutputEvent(data.toString(), "stderr"))
                );

                child.stdout.on('data', data =>
                    this.sendEvent(new OutputEvent(data.toString(), "stdout"))
                );
                break;
        }

        this.sendEvent(new OutputEvent("Debugger launchRequest - Sent response\n"));
        this.sendResponse(response);
    }


    protected attachRequest(response: DebugProtocol.AttachResponse, args: AttachRequestArguments, request?: DebugProtocol.Request): void {

        var port = args.port ? args.port : DEBUG_PORT;
        this.justStart = true;
        this.sourcePaths = [];

        if ("workingDir" in args) {
            this.sourcePaths.push(args.workingDir);
        }

        if ("sourcePaths" in args) {
            this.sourcePaths = this.sourcePaths.concat(args.sourcePaths);
        }

        this.Debugging = !args.noDebug;
        this.startGo = true;

        let server = net.createServer(socket => {
            this.zh_evaluateClient(socket, server, args)
        }).listen(port, "127.0.0.1" );

        this.sendResponse(response);
    }

    protected disconnectRequest(response: DebugProtocol.DisconnectResponse, args: DebugProtocol.DisconnectArguments, request?: DebugProtocol.Request): void {
        this.zh_command("DISCONNECT\r\n");
        this.sendResponse(response);
    }

    protected terminateRequest(response: DebugProtocol.TerminateResponse, args: DebugProtocol.TerminateArguments, request?: DebugProtocol.Request) {
        process.kill(this.processId, 'SIGKILL');
        this.sendResponse(response);
    }


    private zh_evaluateClient(socket: net.Socket, server: net.Server, args: any) {

        let nData = 0;
        let exeTarget = path.basename(args.program, path.extname(args.program)).toLowerCase();

        socket.on("data", (data: Buffer) => {

            this.sendEvent(new OutputEvent(`socket.on data, data.toString()=${data.toString()}\n`));

            if (this.socket && nData > 0) {
                this.zh_processInput(data.toString());
                return;
            }
            if (nData > 0) {
                return;
            }
            nData++;

            // the client sent exe name and process ID		
            var lines = data.toString().split("\r\n");

            if (lines.length < 2) //todo: check if they arrive in 2 tranches.
                return;

            let clPath = path.basename(lines[0], path.extname(lines[0])).toLowerCase();
            if (clPath != exeTarget) {
                socket.write("NO\r\n");
                socket.end();
                return;
            }
            socket.write("HELLO\r\n");
            this.zh_setProcess(parseInt(lines[1]));

            //connected!
            this.sendEvent(new InitializedEvent());
            server.close();
            this.socket = socket;

            socket.removeAllListeners("data");
            socket.on("data", (data: Buffer) => {
                this.zh_processInput(data.toString())
            });

            socket.write(this.queue);
            this.justStart = false;
            this.queue = "";
        });
    }


    // line type ?
    private zh_sendVariables(id: any ) { //, cLineX: string) {

        //this.sendEvent(new OutputEvent(`zh_sendVariables id=${id} cLineX=${cLineX}\n`));
        this.sendEvent(new OutputEvent(`zh_sendVariables id=${id}\n`));


        let vars = [];
        this.zhProcessLineCallback = (cL: string) => {

            this.sendEvent(new OutputEvent(`zh_sendVariables/zhProcessLineCallback id=${id} cL=${cL} vars.length=${vars.length}\n`));

            if (cL.startsWith("END")) {
                this.varResp[id].body = {
                    variables: vars
                };
                this.sendResponse(this.varResp[id]);
                this.zhProcessLineCallback = undefined;
                return;
            }

            let aInfos: string[] = cL.split( ":" );
            cL = aInfos[0] + ":" + aInfos[1] + ":" + aInfos[2] + ":" + aInfos[3];

             //the value can contains : , we need to rejoin it.
            if (aInfos.length > 7) {
                aInfos[6] = aInfos.splice(6).join(":");
            }
            let debuggerVariable = new Variable( /*name*/ aInfos[4], /*value*/ aInfos[6]) // /*ref*/ cLine);
            debuggerVariable = this.zh_getVariableFormat(debuggerVariable, aInfos[5], aInfos[6], "value", cL, id);
            vars.push(debuggerVariable);
        }
    }

    private zh_sendScope(inError: boolean) {

        // reset references
        this.variableReferences = [];

        if (inError)
            this.variableReferences.push("ERROR_VAR");

        this.variableReferences = this.variableReferences.concat([
            "LOCALS",
            "PUBLICS",
            "PRIVATES",
            "PRIVATE_CALLEE",
            "STATICS"
        ]);

        //TODO: "GLOBALS","EXTERNALS"
        this.varResp = [];
        this.varResp.length = this.variableReferences.length;
        this.variableEvaluations = [];
        this.variableEvaluations.length = this.variableReferences.length;

        let n = 0;
        let scopes: Scope[] = [];

        if (inError) {
            n = 1;
            scopes.push(new Scope("Error", 1));
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
        let response = this.scopeResponse;
        response.body = { 
            scopes: scopes 
        };
        this.sendResponse(response);
    }


    private zh_getVarReference(cLine: string, evaluate: any) {

        let r = this.variableReferences.indexOf(cLine);
        if (r >= 0)
            return r + 1;

        let aInfos = cLine.split(":");
        //the value can contains : , we need to rejoin it.
        if (aInfos.length > 4) {
            aInfos[2] = aInfos.splice(2).join(":").slice(0, -1);
            aInfos.length = 3;
            cLine = aInfos.join(":") + ":";
        }

        this.variableReferences.push(cLine);
        this.variableEvaluations.push(evaluate);
        this.sendEvent(new OutputEvent(`zh_getVarReference variable cLine=${cLine} evaluate=${evaluate}\n`));
        return this.variableReferences.length;
    }

    
    private zh_getVariableFormat(debuggerVariable: Variable | string | any, type: string, value: string, valueName:string, cLine: string, id?: any): any {

        // zh_getVariableFormat: debuggerVariable={"name":"S_ASQLCONNECTIONS","value":"0","variablesReference":null} ; type=A ; value=0 ; valueName=value line=NaN id=4 
        // zh_getVariableFormat: debuggerVariable={"name":"CQUERY","value":"\"SELECT public.fetchmetrictext('kalk_cache_tabela/knjig')\"","variablesReference":null} ; type=C ; value="SELECT public.fetchmetrictext('kalk_cache_tabela/knjig')" ; valueName=value line=NaN id=0 

        this.sendEvent(new OutputEvent(`zh_getVariableFormat: debuggerVariable=${JSON.stringify(debuggerVariable)} ; type=${type} ; value=${value} ; valueName=${valueName} line=${cLine} id=${id} \n`));
        // char
        if (type == "C") {
            value = value.replace(/\\\$\\n/g, "\n");
            value = value.replace(/\\\$\\r/g, "\r");
        }
        debuggerVariable[valueName] = value;
        debuggerVariable.type = type;

        if (["E", "B", "P"].indexOf(debuggerVariable.type) == -1) {

            debuggerVariable.evaluateName = "";
            if (this.variableEvaluations[id])
                debuggerVariable.evaluateName = this.variableEvaluations[id];

            debuggerVariable.evaluateName += debuggerVariable.name;
            if (this.variableEvaluations[id] && this.variableEvaluations[id].endsWith( "[" ))
                debuggerVariable.evaluateName += "]";
        }
        switch (type) {

            // array
            case "A":
                debuggerVariable.variablesReference = this.zh_getVarReference(cLine, debuggerVariable.evaluateName + "[");
                debuggerVariable[valueName] = `ARRAY(${value})`;
                debuggerVariable.indexedVariables = parseInt(value);
                break;

            // hash
            case "H":
                debuggerVariable.variablesReference = this.zh_getVarReference(cLine, debuggerVariable.evaluateName + "[");
                debuggerVariable[valueName] = `HASH(${value})`;
                debuggerVariable.namedVariables = parseInt(value);
                break;

            // object
            case "O":
                debuggerVariable.variablesReference = this.zh_getVarReference(cLine, debuggerVariable.evaluateName + ":");
                let infos = value.split(" ");
                debuggerVariable[ valueName ] = `CLASS ${infos[0]}`;
                debuggerVariable.namedVariables = parseInt(infos[1]);
                break;
        }
        //zh_getVariableFormat: OUT debuggerVariable={"name":"CQUERY","value":"\"SELECT public.fetchmetrictext('kalk_cache_tabela/knjig')\"","variablesReference":null,"type":"C","evaluateName":"CQUERY"} ; type=C ; value="SELECT public.fetchmetrictext('kalk_cache_tabela/knjig')" ; valueName=value line=NaN id=0
        this.sendEvent(new OutputEvent(`zh_getVariableFormat: OUT debuggerVariable=${JSON.stringify(debuggerVariable)} ; type=${type} ; value=${value} ; valueName=${valueName} line=${cLine} id=${id} \n`));
        
        return debuggerVariable;
    }

    /**
     * Evaluate the return from an expression request
     * line the income line
     */
    private zh_processExpression(cLine: string) {

        // EXPRESSION:{frame}:{type}:{result}
        var aInfos = cLine.split(":");
        //the value can contains : , we need to rejoin it.
        if (aInfos.length > 4) {
            aInfos[3] = aInfos.splice(3).join(":");
        }

        let resp = this.evaluateResponses.shift();
        cLine = "EXP:" + aInfos[1] + ":" + resp.body.result.replace(/:/g, ";") + ":";
        resp.body.name = resp.body.result
        if (aInfos[2] == "E") {
            resp.success = false;
            resp.message = aInfos[3];
        } else
            resp.body = this.zh_getVariableFormat(resp.body, aInfos[2], aInfos[3], "result", cLine);
        this.sendResponse(resp);
    }


    private zh_sendStack(cLine: string) {

        var nStack = parseInt(cLine.substring(6));
        this.sendEvent(new OutputEvent(`zh_sendStack cLine=${cLine} nStack=${nStack}\n`));
        
        let frames = [];
        frames.length = nStack;
        var j = 0;

        this.zhProcessLineCallback = (cL: string) => {

            this.sendEvent(new OutputEvent(`zh_sendStack/zhProcessLineCallback cL=${cL}\n`));
            let infos = cL.split(":");
            for (let i = 0; i < infos.length; i++) 
               infos[i] = infos[i].replace(";", ":");

            let completePath = infos[0];
            let found = false;
            if (path.isAbsolute(infos[0]) && fs.existsSync(infos[0])) {
                completePath = this.zh_getRealPath(infos[0]);
                found = true;
            } else
                for (let i = 0; i < this.sourcePaths.length; i++) {
                    if (fs.existsSync(path.join(this.sourcePaths[i], infos[0]))) {
                        completePath = this.zh_getRealPath(path.join(this.sourcePaths[i], infos[0]));
                        found = true;
                        break;
                    }
                }
            
            if (found)
                infos[0] = path.basename(completePath);

            frames[j] = new StackFrame(j, infos[2], new Source(infos[0], completePath), parseInt(infos[1]));
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
                this.zhProcessLineCallback = undefined;
            }
        }
    }

    private zh_processBreak(cLine: string) {

        let aInfos: any[] = cLine.split(":");
        
        if ( !(aInfos[1] in this.breakpoints)) {
            //error 
            return;
        }
        aInfos[2] = parseInt(aInfos[2]);
        aInfos[3] = parseInt(aInfos[3]);
        let oBreakPoint = this.breakpoints[aInfos[1]];
        this.sendEvent(new OutputEvent(`zh_processBreak ${cLine}; aInfos=${JSON.stringify(aInfos)} \n`));

        let idBreak = oBreakPoint.response.body.breakpoints.findIndex( 
            (b: any) => b.line == aInfos[2]
        );

        if (idBreak == -1) {
            // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/in
            if (aInfos[2] in oBreakPoint) {
                delete oBreakPoint[aInfos[2]];
                this.zh_checkBreakPoint(aInfos[1]);
            }
            return;
        }

        if (aInfos[3] > 1) {
            oBreakPoint.response.body.breakpoints[idBreak].line = aInfos[3];
            oBreakPoint.response.body.breakpoints[idBreak].verified = true;
            oBreakPoint[aInfos[2]] = 1;
        } else {
            oBreakPoint.response.body.breakpoints[idBreak].verified = false;
            if (aInfos[4] == 'notfound')
                //dest.response.body.breakpoints[idBreak].message = localize('ziher.dbgNoModule')
                oBreakPoint.response.body.breakpoints[idBreak].message = "Module not found";
            else
                // dest.response.body.breakpoints[idBreak].message = localize('ziher.dbgNoLine')
                oBreakPoint.response.body.breakpoints[idBreak].message = "Invalid line";

            oBreakPoint[aInfos[2]] = 1;
        }
        this.zh_checkBreakPoint(aInfos[1]);
    }

    private zh_checkBreakPoint(src: string) {

        let oBreakPoint = this.breakpoints[src];
        this.sendEvent(new OutputEvent(`zh_checkBreakPoint src=${src} oBreakPoint=${JSON.stringify(oBreakPoint)}\n`));
        
        for (let cKey in oBreakPoint) {
            if (oBreakPoint.hasOwnProperty(cKey) && cKey != "response") {
                if (oBreakPoint[cKey] != 1) {
                    return;
                }
            }
        }

        this.sendEvent(new OutputEvent(`zh_checkBreakPoint dest.response=${oBreakPoint.response}\n`));
        this.sendResponse(oBreakPoint.response);
    }


    private zh_processInput(buff: string) {

        let aLines = buff.split("\r\n");
        for (let nI: number = 0; nI < aLines.length; nI++) {

            let cLine = aLines[nI];
            // zh_processInput i=3 cLine=Z18/src/Z18-klijent.zh:57:MAIN
            this.sendEvent(new OutputEvent(`zh_processInput i=${nI} cLine=${cLine}\n`));

            if (cLine.length == 0)
                continue;

            if (this.zhProcessLineCallback) {
                this.zhProcessLineCallback(cLine);
                continue;
            }
            if (cLine.startsWith("STOP")) {
                this.sendEvent(new StoppedEvent(cLine.substring(5), 1));
                continue;
            }
            if (cLine.startsWith("STACK")) {
                this.zh_sendStack(cLine);
                continue;
            }
            if (cLine.startsWith("BREAK")) {
                this.zh_processBreak(cLine);
                continue;
            }
            if (cLine.startsWith("ERROR") && !cLine.startsWith("ERROR_VAR")) {
                let stopEvt = new StoppedEvent("error", 1, cLine.substring(6));
                this.sendEvent(stopEvt);
                continue;
            }
            if (cLine.startsWith("EXPRESSION")) {
                this.zh_processExpression(cLine);
                continue;
            }
            if (cLine.startsWith("LOG")) {
                continue;
            }
            if (cLine.startsWith("INERROR")) {
                this.zh_sendScope(cLine[8] == 'T')
                continue;
            }
            if (cLine.startsWith("COMPLETITION")) {
                this.zh_processCompletion(cLine);
                continue;
            }
            let nJ: number;
            for (nJ = this.variableReferences.length - 1; nJ >= 0; nJ--) {

                if (cLine.startsWith(this.variableReferences[nJ])) {
                    // zh_processInput variableCommands[j]=LOCALS cLine=LOCALS 1
                    this.sendEvent(new OutputEvent(`zh_processInput variableCommands[j]=${this.variableReferences[nJ]} cLine=${cLine}\n`));
                    this.zh_sendVariables(nJ) //, cLine);
                    break;
                }
            }
            if (nJ != this.variableReferences.length)
                continue;
        }
    }

    protected continueRequest(response: DebugProtocol.ContinueResponse, args: DebugProtocol.ContinueArguments, request?: DebugProtocol.Request): void {
        this.zh_command("GO\r\n");
        this.sendResponse(response);
    }

    protected nextRequest(response: DebugProtocol.NextResponse, args: DebugProtocol.NextArguments, request?: DebugProtocol.Request): void {
        this.zh_command("NEXT\r\n");
        this.sendResponse(response);
    }

    protected stepInRequest(response: DebugProtocol.StepInResponse, args: DebugProtocol.StepInArguments, request?: DebugProtocol.Request): void {
        this.zh_command("STEP\r\n");
        this.sendResponse(response);
    }

    protected stepOutRequest(response: DebugProtocol.StepOutResponse, args: DebugProtocol.StepOutArguments, request?: DebugProtocol.Request): void {
        this.zh_command("EXIT\r\n");
        this.sendResponse(response);
    }

    protected pauseRequest(response: DebugProtocol.PauseResponse, args: DebugProtocol.PauseArguments, request?: DebugProtocol.Request): void {
        this.zh_command("PAUSE\r\n");
        this.sendResponse(response);
    }

    protected threadsRequest(response: DebugProtocol.ThreadsResponse, request?: DebugProtocol.Request): void {

        response.body =
        {
            threads:
                [ //TODO: suppport multi thread
                    new Thread(1, "Main Thread")
                ]
        };
        this.sendResponse(response);
    }

    private zh_processCompletion(line: any) {

        this.zhProcessLineCallback = (cL: string) => {
            
            this.sendEvent(new OutputEvent(`zh_processCompletion/zhProcessLineCallback cL=${cL}\n`));

            if (cL == "END") {
                this.sendResponse(this.completionsResponse);
                this.zhProcessLineCallback = undefined;
                return;
            }
            if (!this.completionsResponse.body)
                this.completionsResponse.body = {
                    targets: []
                };

            if (!this.completionsResponse.body.targets)
                this.completionsResponse.body.targets = [];

            let type = cL.substr(0, line.indexOf(":"));
            cL = line.substr(line.indexOf(":") + 1);

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


    protected setBreakPointsRequest(response: DebugProtocol.SetBreakpointsResponse, args: DebugProtocol.SetBreakpointsArguments, request?: DebugProtocol.Request): void {

        var message = "";
        // prepare a response
        response.body = { "breakpoints": [] };
        response.body.breakpoints = [];
        response.body.breakpoints.length = args.breakpoints.length;
        // check if the source is already configurated for breakpoints
        let src = args.source.name.toLowerCase();
        let dest
        if (!(src in this.breakpoints)) {
            this.breakpoints[src] = {};
        }
        // mark all old breakpoints for deletion
        dest = this.breakpoints[src];
        for (var i in dest) {
            if (dest.hasOwnProperty(i)) {
                dest[i] = "-" + dest[i];
            }
        }
        // check current breakpoints
        dest.response = response;
        for (let i: number = 0; i < args.breakpoints.length; i++) {

            var breakpoint = args.breakpoints[i];
            response.body.breakpoints[i] = new Breakpoint(false, breakpoint.line);
            var thisBreakpoint = "BREAKPOINT\r\n";
            thisBreakpoint += `+:${src}:${breakpoint.line}`;

            if ('condition' in breakpoint && breakpoint.condition.length > 0) {
                thisBreakpoint += `:?:${breakpoint.condition.replace(/:/g, ";")}`
            }

            if ('hitCondition' in breakpoint) {
                thisBreakpoint += `:C:${breakpoint.hitCondition}`
            }

            if ('logMessage' in breakpoint) {
                thisBreakpoint += `:L:${breakpoint.logMessage.replace(/:/g, ";")}`
            }

            if (dest.hasOwnProperty(breakpoint.line) && dest[breakpoint.line].substring(1) == thisBreakpoint) { // breakpoint already exists
                dest[breakpoint.line] = thisBreakpoint
                response.body.breakpoints[i].verified = true;
            } else {
                //require breakpoint
                message += thisBreakpoint + "\r\n"
                dest[breakpoint.line] = thisBreakpoint;
            }
        }

        // require delete old breakpoints
        let n1 = 0;
        for (let i in dest) {
            if (dest.hasOwnProperty(i) && i != "response") {
                if (dest[i].substring(0, 1) == "-") {
                    message += "BREAKPOINT\r\n";
                    message += `-:${src}:${i}\r\n`;
                    dest[i] = "-";
                }
            }
        }
        this.zh_checkBreakPoint(src);
        this.zh_command(message);
        //this.sendResponse(response);
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

    protected evaluateRequest(response: DebugProtocol.EvaluateResponse | any, args: DebugProtocol.EvaluateArguments, request?: DebugProtocol.Request): void {

        response.body = {};
        this.sendEvent(new OutputEvent(`evaluateRequest: response=${response} args=${JSON.stringify(args)}\n`));
        

        response.body.result = args.expression;
        this.sendEvent(new OutputEvent(`evaluateRequest: OUT push response=${response}\n`));

        this.evaluateResponses.push(response);

        this.sendEvent(new OutputEvent(`zh_command: EXPRESSION\r\n${args.frameId + 1 || this.currentStack}:${args.expression.replace(/:/g, ";")}\r\n`));
        this.zh_command(`EXPRESSION\r\n${args.frameId + 1 || this.currentStack}:${args.expression.replace(/:/g, ";")}\r\n`);
    }


    protected variablesRequest(response: DebugProtocol.VariablesResponse, args: DebugProtocol.VariablesArguments, request?: DebugProtocol.Request) {

        var zhStart = args.start ? args.start + 1 : 1;
        var zhCount = args.count ? args.count : 0;
        //var prefix;
        if (args.variablesReference <= this.variableReferences.length) {
            this.varResp[args.variablesReference - 1] = response;
            this.zh_command(`${this.variableReferences[args.variablesReference - 1]}\r\n` +
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

    private zh_setProcess(pid: number) {

        //var tc = this;
        this.processId = pid;
        var interval = setInterval(() => {
            try {
                process.kill(pid, 0);
            } catch (error) {
                this.sendEvent(new TerminatedEvent());
                clearInterval(interval);
            }
        }, 1000);

    }



}