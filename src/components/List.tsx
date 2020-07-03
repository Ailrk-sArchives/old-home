import {Markdown, chronoList, indexTag} from '../state/markdowns';
import {Container, Col, Badge} from 'react-bootstrap';
import React from 'react';
import {css} from 'glamor';
import {textTheme, linkStyle} from '../styles/styleElements';
import {Link, useParams} from 'react-router-dom';

const listRowStyle = css(textTheme, {
  paddingBottom: "50px",
});

function ListRow(props: {
  markdown: Markdown
}) {
  const {markdown} = props;
  const {header} = markdown;
  const {id, title, tag, time} = header;

  return (
    <Container {...listRowStyle}>
      <Col>
        <Link to={`/article/${id}`} style={{...linkStyle, color: "LightCoral"}}>
          <h2><b>{title}</b></h2>
        </Link>
      </Col>
      <Col {...css({fontSize: 20, paddingLeft: "50px", color: "SlateGray"})}>
        {time.toJSON().replace(/-/gi, '.').split('T')[0]}
      </Col>
      <Col {...css({paddingLeft: "45px"})}>
        <h4>
          {
            tag?.map(t => (
              <span key={t}>
                <Badge variant="light">
                  <Link to={`/tag/${t}`} style={{...linkStyle, color: "SlateGray"}}>
                    {t}
                  </Link>
                </Badge>
                    &nbsp;
              </span>
            ))
          }
        </h4>
      </Col>
    </Container>
  );
}

export function List(props: {
  markdowns: Array<Markdown>,
}) {
  const {markdowns} = props;
  const lists = markdowns.map(m => <ListRow markdown={m} key={m.header.id} />);
  return (
    <Container>
      {
        lists
      }
    </Container>
  );
}

export function ChronoList() {
  return <List markdowns={chronoList()} />
}

export function TagList() {
  const {tag} = useParams();
  return <List markdowns={indexTag.get(tag as string) ?? []} />
}
