import React from 'react';
import {BrowserRouter, Switch, Route} from 'react-router-dom';
import {ArticlePage} from './Article';
import {Header} from './Header';
import {ChronoList, TagList} from './List';
import {Tags} from './Tags';
import 'highlightjs/styles/github.css';

function App() {

  return (
    <BrowserRouter basename={process.env.PUBLIC_URL}>
      <Header />
      <Switch>
        <Route exact path="/article/:id" component={ArticlePage} />
        <Route exact path="/" component={ChronoList} />
        <Route exact path="/tag/:tag" component={TagList} />
        <Route exact path="/tags" component={Tags} />
      </Switch>
    </BrowserRouter>
  );
}

export default App;
