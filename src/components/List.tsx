import {Markdown, allDB, chronoLists} from '../state/markdowns';
import {Container} from 'react-bootstrap';
import React, {useState, useEffect} from 'react';
import {useParams} from 'react-router-dom';
import {ItemRow} from './ItemRow';
import {useDelayRender} from '../state/hooks';
import "./List.css";

export function List(props: {
  markdowns: Array<Markdown>,
}) {
  const {markdowns} = props;
  const delay = useDelayRender(0.1);
  const lists =
    markdowns.map(m =>
      <ItemRow markdown={m} key={m.header.id} />);
  return (
    <Container>
      <div className={"list-main"}
        style={{visibility: delay ? "hidden" : "visible", }} />
      {
        lists
      }
    </Container>
  );
}

// Monomophization of list.
export function ArticleChronoList() {
  return <List markdowns={chronoLists.articleChronoList} />
}

export function NotesChronoList() {
  return <List markdowns={chronoLists.noteChronoList} />
}

export function OthersChronoList() {
  return <List markdowns={chronoLists.otherChronoList} />
}

export function ReportChronoList() {
  return <List markdowns={chronoLists.noteChronoList} />
}

export function TagList() {
  const {tag} = useParams();
  return <List markdowns={allDB.get(tag as string) ?? []} />
}
