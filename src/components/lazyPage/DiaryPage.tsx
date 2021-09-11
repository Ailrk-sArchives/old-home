import React from 'react';
import { Article } from '../Article';
import { diaryDB } from '../../state/markdowns';
import { useParams } from 'react-router-dom';
import "./DiaryPage.css";


export default () => {
  const { id } = useParams();
  const markdown = diaryDB.get(Number.parseInt(id as string));
  return (<Article markdown={markdown} className="diary-page" />);
}
