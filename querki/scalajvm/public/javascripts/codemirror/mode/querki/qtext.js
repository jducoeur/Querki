(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"), require("../meta"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror", "../meta"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

CodeMirror.defineSimpleMode("qtext", {
  start: [
    {regex: /\[\[/, token: "delimit-ql", mode: {spec: "ql", end: /\]\]/ }}
  ],
  meta: {
  }
});

CodeMirror.defineMIME("text/x-qtext", "qtext");

});
