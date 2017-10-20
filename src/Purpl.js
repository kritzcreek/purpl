var vm = require('vm');
var util = require('util');

function purplEval(code, ctx) {
    var context = ctx || vm.createContext({});
    var script = new vm.Script("(function(require){ return (" + code + ") })");
    var result = script.runInContext(context)(require);
    return { context: context, result: result };
}

function purplEvalAsync(code, cb, ctx) {
    var context = ctx || vm.createContext({});
    var newCb = function (result) {
        cb({ context: context, result: result });
    };
    var script = new vm.Script("(function(require){ return " + code + " })");
    script.runInContext(context)(require)(newCb);
}

exports.jsonparse = function(s) { return JSON.parse(s); };
exports.purplEval = purplEval;
exports.purplEvalAsync = purplEvalAsync;
exports.createContext = function(r) {
    return function() {
        vm.createContext(r);
    };
};

// const {result, context} = purplEval(expr)
// var expr = 'dude = require("./extern.js").exp';
// console.log(util.inspect(purplEval('dude', context).result))
// purplEvalAsync('(function(k) { rofl = x; k(dude) })', r => console.log(util.inspect(r)), context)
