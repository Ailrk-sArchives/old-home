import {AddPageTitle} from '../Misc';
import {ArticleChronoList} from '../List';


export const ArticleChronoListPage: React.FC<{}> = () =>
  <AddPageTitle pageTitle={"Articles"} page={<ArticleChronoList />} />;


