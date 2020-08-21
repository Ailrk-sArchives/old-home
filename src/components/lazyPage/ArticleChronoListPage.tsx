import React from 'react';
import {ArticleChronoList} from '../List'
import {AddPageTitle} from '../Misc';

// ArticleChronoListPage
export default function () {
  return <AddPageTitle pageTitle={"Articles"} page={<ArticleChronoList />} />;
}
