import React from 'react';
import {Article} from './Article';
import {papersDB} from '../state/markdowns';
import {Container} from 'react-bootstrap';

export function Paper() {
  return (
    <Container>
      <Article markdown={papersDB.values("default")?.[0]} />
    </Container>
  );
}
