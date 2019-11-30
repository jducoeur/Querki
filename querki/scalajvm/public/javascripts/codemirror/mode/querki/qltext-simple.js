(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"), require("../meta"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror", "../meta"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

CodeMirror.defineSimpleMode("qltext", {
  start: [
    {regex: /\[\[/, token: "delimit-ql", push: "ql"},
    {regex: '""', token: "delimit-qtext", pop: true}
  ],

  ql: [
    {regex: /\]\]/, token: "delimit-ql", pop: true},
    {regex: '->', token: "ql-syntax"},
    {regex: ',', token: "ql-syntax"},
    {regex: /[\(\)]/, token: "ql-syntax"},
    {regex: '""', token: "delimit-qtext", push: "start"},
    {regex: /[A-Za-z0-9_ ]+/, token: "ql-name"}
  ],
  meta: {
  }
});

CodeMirror.defineMIME("text/x-qltext", "qltext");

});
