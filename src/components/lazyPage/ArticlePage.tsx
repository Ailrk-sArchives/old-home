import React from 'react';
import {Article} from '../Article';
import {allDB} from '../../state/markdowns';
import {useParams} from 'react-router-dom';

export default function ArticlePage() {
  const {id} = useParams();
  const markdown = allDB.get(Number.parseInt(id as string));
  return (<Article markdown={markdown} />);
}


