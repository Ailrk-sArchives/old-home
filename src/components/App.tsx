import React from 'react';
import {HashRouter, Switch, Route} from 'react-router-dom';
import {Header} from './Header';
import {About} from './About';
import {
  ArticlePage,
  ArticleChronoListPage,
  NoteChronoListPage,
  OthersChronoListPage,
  TagListPage,
  TagsPage,
} from './Page';

import 'highlightjs/styles/github.css';

function App() {
  // const [test, setTest] = useState<string>("Not found");
  // useEffect(() => {
  //   axios.get<string>(process.env.PUBLIC_URL + "/test.html")
  //     .then(t => {
  //       setTest(t.data);
  //       console.log(process.env.PUBLIC_URL + "/test.html");
  //     });
  // }, []);
  // console.log(test);

  return (
    <HashRouter>
      <Header />
      <Switch>
        <Route exact path="/article/:id" component={ArticlePage} />
        <Route exact path="/" component={ArticleChronoListPage} />
        <Route exact path="/about" component={About} />
        <Route exact path="/tag/:tag" component={TagListPage} />
        <Route exact path="/tags" component={TagsPage} />
        <Route exact path="/notes" component={NoteChronoListPage} />
        <Route exact path="/others" component={OthersChronoListPage} />
      </Switch>
    </HashRouter>
  );
}

export default App;
