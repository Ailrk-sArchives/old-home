import {Markdown, chronoList, articlesDB} from '../state/markdowns';
import {Container, Col, Badge} from 'react-bootstrap';
import React from 'react';
import {css} from 'glamor';
import {textTheme, linkStyle} from '../styles/styleElements';
import {Link, useParams} from 'react-router-dom';
import {HoverLink} from './Misc';

const listRowStyle = css(textTheme, {
  paddingTop: "20px",
  paddingBottom: "30px",
  marginBottom: "50px",
  borderLeft: "15px solid LightCoral",
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
        <h3>
          <HoverLink text={title}
            link={`/#/article/${id}`}
            ogColor={"DimGrey"}
            onHoverColor={"LightCoral"} />
        </h3>
      </Col>
      <Col {...css({fontSize: 20, paddingLeft: "50px", color: "LightCoral", fontWeight: "bold"})}>
        {time.toJSON().replace(/-/gi, '.').split('T')[0]}
      </Col>
      <Col {...css({paddingLeft: "45px"})}>
        <h4>
          {
            tag?.map(t => (
              <span key={t}>
                <Badge variant="light">
                  <Link to={`/home/tag/${t}`} style={{...linkStyle, color: "LightCoral"}}>
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
  return <List markdowns={articlesDB.get(tag as string) ?? []} />
}
