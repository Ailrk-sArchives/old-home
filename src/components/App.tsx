import React, {Suspense, lazy} from 'react';
import {Link} from 'react-router-dom';
import {HashRouter, Switch, Route} from 'react-router-dom';
import {HeaderSimple} from './HeaderSimple';
import {Container} from 'react-bootstrap';
import {useWindowSize} from '../state/hooks';
import {LoaderSpinner} from './Misc';
import {
  AboutMePage,
  TagListPage,
  TagPage,
  FriendsPage,
  InterestingLinksPage
} from './Page';

import 'highlightjs/styles/github.css';
import '../styles/general.css';

import QRIter1 from '../assets/iter1.png';
import QRIter2 from '../assets/iter2.png';

const ArticlePage: any = lazy(() => import("./lazyPage/ArticlePage"));
const ArticleChronoListPage: any = lazy(() => import("./lazyPage/ArticleChronoListPage"));
// const NoteChronoListPage: any = lazy(() => import("./lazyPage/NoteChronoListPage"));
// const OthersChronoListPage: any = lazy(() => import("./lazyPage/OthersChronoListPage"));

const Iter = (url: string, qriter: string) => () => (
  <Container>
    <Link to={url}>
      <img src={qriter} />
    </Link>
  </Container>
);
const Iter2 = Iter("iter2", QRIter2)
const Iter1 = Iter("iter1", QRIter1)

function App() {

  const {width} = useWindowSize();
  const header = <HeaderSimple />

  return (
    <HashRouter>
      <Switch>
        <Suspense fallback={LoaderSpinner}>
          {
            header
          }
          <Route exact path="/article/:id" component={ArticlePage} />
          <Route exact path="/" component={ArticleChronoListPage} />
          <Route exact path="/about" component={AboutMePage} />
          <Route exact path="/tag/:tag" component={TagListPage} />
          <Route exact path="/tags" component={TagPage} />
          <Route exact path="/friends" component={FriendsPage} />
          <Route exact path="/links" component={InterestingLinksPage} />
          <Route exact path="/iter2" component={Iter1} />
          <Route exact path="/iter1" component={Iter2} />
        </Suspense>
        <Route exact path="/about" component={AboutMePage} />
      </Switch>
    </HashRouter>
  );
}

export default App;
