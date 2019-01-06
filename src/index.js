require('./styles.css');
var main = require('./Main');
var Elm = main.Elm;

var session = localStorage.getItem('session');
if (session) {
  session = JSON.parse(session);
} else {
  session = {};
}

var pageData = localStorage.getItem('pageData');
if (pageData) { session.pageData = JSON.parse(pageData); }

var rootUrl = window.location.protocol + "//" + window.location.host + "/";

var app = Elm.Main.init({
  flags: {
    rootUrl: rootUrl,
    session: session
  }
});

app.ports.storePageData.subscribe(function(data) {
  localStorage.setItem('pageData', JSON.stringify(data.pageData));

  if (data.clearSession) {
    localStorage.removeItem('session');
  }
});

app.ports.storeSession.subscribe(function(data) {
  var pageData = data.pageData;
  delete data.pageData;

  localStorage.setItem('pageData', JSON.stringify(pageData));
  localStorage.setItem('session', JSON.stringify(data));
});

app.ports.clearSession.subscribe(function() {
  localStorage.removeItem('pageData');
  localStorage.removeItem('session');
});
