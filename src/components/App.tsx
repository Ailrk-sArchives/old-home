import React from 'react';
import {BrowserRouter, Switch, Route} from 'react-router-dom';
import {ArticlePage} from './Article';
import {Header} from './Header';
import {ChronoList, TagList} from './List';

function App() {

  return (
    <BrowserRouter>
      <Header />
      <Switch>
        <Route exact path="/article/:id" component={ArticlePage} />
        <Route exact path="/home" component={ChronoList} />
        <Route exact path="/tag/:tag" component={TagList} />
      </Switch>
    </BrowserRouter>
  );
}

export default App;
