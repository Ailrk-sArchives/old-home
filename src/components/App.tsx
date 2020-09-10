import React, {Suspense, lazy} from 'react';
import {HashRouter, Switch, Route} from 'react-router-dom';
import {Header, CollapsedHeader} from './Header';
import {useWindowSize} from '../state/hooks';
import {LoaderSpinner} from './Misc';
import {
  AboutMePage,
  TagListPage,
  TagsPage,
  PaperPage,
} from './Page';

import 'highlightjs/styles/github.css';


const ArticlePage: any = lazy(() => import("./lazyPage/ArticlePage"));
const ArticleChronoListPage: any = lazy(() => import("./lazyPage/ArticleChronoListPage"));
const NoteChronoListPage: any = lazy(() => import("./lazyPage/NoteChronoListPage"));
const OthersChronoListPage: any = lazy(() => import("./lazyPage/OthersChronoListPage"));

function App() {

  const {width} = useWindowSize();
  const header = width > 1000 ? <Header /> : <CollapsedHeader />;

  return (
    <HashRouter>
      {
        header
      }
      <Switch>
        <Suspense fallback={LoaderSpinner}>
          <Route exact path="/article/:id" component={ArticlePage} />
          <Route exact path="/" component={ArticleChronoListPage} />
          <Route exact path="/about" component={AboutMePage} />
          <Route exact path="/tag/:tag" component={TagListPage} />
          <Route exact path="/tags" component={TagsPage} />
          <Route exact path="/notes" component={NoteChronoListPage} />
          <Route exact path="/reports" component={OthersChronoListPage} />
          <Route exact path="/paper" component={PaperPage} />
        </Suspense>
        <Route exact path="/about" component={AboutMePage} />
      </Switch>
    </HashRouter>
  );
}

export default App;
