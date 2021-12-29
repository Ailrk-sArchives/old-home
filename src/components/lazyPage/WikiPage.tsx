import React from 'react';
import { Article } from '../Article';
import { wikiDB } from '../../state/markdowns';
import { useParams } from 'react-router-dom';
import "./WikiPage.css";


export default () => {
  const { id } = useParams();
  const markdown = wikiDB.get(Number.parseInt(id as string));
  return (<Article markdown={markdown} className="wiki-page" />);
}
