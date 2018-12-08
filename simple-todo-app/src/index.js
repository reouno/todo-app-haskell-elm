'usr strict'; // ECMAScript 5 Strict Mode : https://johnresig.com/blog/ecmascript-5-strict-mode-json-and-more/

require('ace-css/css/ace.css');
require('font-awesome/css/font-awesome.css');

require('./index.html');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

var app = Elm.Main.embed(mountNode);
