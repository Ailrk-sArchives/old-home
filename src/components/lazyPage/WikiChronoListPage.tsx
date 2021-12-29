import React from 'react';
import {WikiChronoList} from '../List'
import {AddPageTitle} from '../Misc';

// ArticleChronoListPage
export default function () {
  return <AddPageTitle pageTitle={"Articles"} page={<WikiChronoList />} />;
}
