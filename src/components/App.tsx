import React from 'react';
import {HashRouter, Switch, Route} from 'react-router-dom';
import {ArticlePage} from './Article';
import {Header} from './Header';
import {ChronoList, TagList} from './List';
import {Tags} from './Tags';
import {About} from './About';
import 'highlightjs/styles/github.css';

function App() {

  return (
    <HashRouter>
      <Header />
      <Switch>
        <Route exact path="/article/:id" component={ArticlePage} />
        <Route exact path="/" component={ChronoList} />
        <Route exact path="/about" component={About} />
        <Route exact path="/tag/:tag" component={TagList} />
        <Route exact path="/tags" component={Tags} />
      </Switch>
    </HashRouter >
  );
}

export default App;
