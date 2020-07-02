import React from 'react';
import * as MyMarkdown from '../state/markdowns';
import {BrowserRouter, Switch, Route} from 'react-router-dom';
import {ArticlePage} from './Article';
import {Header} from './Header';
import {List} from './List';

function App() {

  return (
    <BrowserRouter>
      <Header />
      <Switch>
        <Route exact path="/article/:id" component={ArticlePage} />
        <Route exact path="/home">
          <div>
            <List markdowns={Array.from(MyMarkdown.db.values())} />
          </div>
        </Route>
      </Switch>
    </BrowserRouter>
  );
}

export default App;
