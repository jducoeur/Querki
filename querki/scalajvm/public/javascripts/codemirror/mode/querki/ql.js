(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"), require("../meta"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror", "../meta"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

CodeMirror.defineMode("ql", function(config) {
  return {
    startState: function () {
      return {};
    },

    token: function (stream, state) {
    },

    innerMode: function(state) {
      if (state.block == htmlBlock) return {state: state.htmlState, mode: htmlMode};
      if (state.localState) return {state: state.localState, mode: state.localMode};
      return {state: state, mode: mode};
    }
  }
});

CodeMirror.defineMIME("text/x-ql", "ql");

});