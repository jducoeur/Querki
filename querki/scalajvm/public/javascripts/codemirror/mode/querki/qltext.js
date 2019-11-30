(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"), require("../meta"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror", "../meta"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

CodeMirror.defineMode("qltext-outer", function(config) {
  return CodeMirror.multiplexingMode(
    CodeMirror.getMode(config, "text/x-qtext"),
    {
      open: '[[',
      close: ']]',
      mode: CodeMirror.getMode(config, "ql"),
      delimStyle: "delimit-ql"
    }
  )
});

});
