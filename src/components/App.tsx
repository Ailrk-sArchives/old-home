import React, {useState, useEffect} from 'react';
import {HashRouter, Switch, Route} from 'react-router-dom';
import {Header, CollapsedHeader} from './Header';
import {About} from './About';
import {useWindowSize} from '../state/hooks';
import {
  ArticlePage,
  AboutMePage,
  ArticleChronoListPage,
  NoteChronoListPage,
  OthersChronoListPage,
  TagListPage,
  TagsPage,
} from './Page';

import 'highlightjs/styles/github.css';

function App() {

  const {width} = useWindowSize();

  return (
    <HashRouter>
      {
        width > 1000 ? <Header /> : <CollapsedHeader />
      }
      <Switch>
        <Route exact path="/article/:id" component={ArticlePage} />
        <Route exact path="/" component={ArticleChronoListPage} />
        <Route exact path="/about" component={About} />
        <Route exact path="/tag/:tag" component={TagListPage} />
        <Route exact path="/tags" component={TagsPage} />
        <Route exact path="/notes" component={NoteChronoListPage} />
        <Route exact path="/others" component={OthersChronoListPage} />
        <Route exact path="/about" component={AboutMePage} />
      </Switch>
    </HashRouter>
  );
}

export default App;
