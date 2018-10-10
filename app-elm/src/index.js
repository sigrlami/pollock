'use strict';

//require('bootstrap-loader');
require("./styles.css");

const storedToken = localStorage.getItem('token');
let elmOpts;

if (storedToken) {
  elmOpts = {
    nodeEnv:      '' , //process.env.NODE_ENV,
    apiBaseUrl:   'https://localhost:3001/', //process.env.API_URL,
    apiVersion:   1,
    loggedInUser: {token:   storedToken}
  }
} else {
  elmOpts = {
    nodeEnv:      '',//process.env.NODE_ENV,
    apiBaseUrl:   'https://localhost:3001/', //process.env.API_URL,
    apiVersion:   1,
    loggedInUser: null
  }
}

var Elm = require('./Main');
var app = Elm.Main.fullscreen(elmOpts);
