import React from 'react';
import {indexTag} from '../state/markdowns';
import {Container, Badge} from 'react-bootstrap';
import {Link} from 'react-router-dom';
import {linkStyle} from '../styles/styleElements';

export function Tags() {
  const tags = Array.from(indexTag.keys());
  return (
    <Container >
      <h3 style={{color: "SlateGray"}}>All Tags</h3>
      <div style={{...linkStyle, fontSize: 25}}>
        {
          tags.map(t =>
            <Badge variant="light">
              <Link to={`/tag/${t}`} style={{...linkStyle, color: "LightCoral"}}>
                {t}
              </Link>
            </Badge>)
        }
      </div>
    </Container>);
}
