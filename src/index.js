require('./styles.css');
const { Elm } = require('./Main');

var session = localStorage.getItem('session');
if (session) {
  session = JSON.parse(session);
} else {
  session = {};
}

var pageData = localStorage.getItem('pageData');
if (pageData) { session.pageData = JSON.parse(pageData); }

var app = Elm.Main.init({
  flags: { session: session }
});

app.ports.storePageData.subscribe(function(data) {
  localStorage.setItem('pageData', JSON.stringify(data));
});

app.ports.storeSession.subscribe(function(data) {
  var pageData = data.pageData;
  delete data.pageData;

  localStorage.setItem('pageData', JSON.stringify(pageData));
  localStorage.setItem('session', JSON.stringify(data));
});
