import {allDB} from '../../state/markdowns';
import {Article} from '../Article';
import {useParams} from 'react-router-dom';

export function ArticlePage() {
  const {id} = useParams();
  const markdown = allDB.get(Number.parseInt(id as string));
  return (<Article markdown={markdown} />);
}
