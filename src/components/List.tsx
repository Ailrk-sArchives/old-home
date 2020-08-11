import {Markdown, chronoList, articlesDB, notesDB, othersDB, allDB} from '../state/markdowns';
import {Container, Col, Badge} from 'react-bootstrap';
import React from 'react';
import {css} from 'glamor';
import {textTheme, linkStyle} from '../styles/styleElements';
import {Link, useParams} from 'react-router-dom';
import {HoverLink} from './Misc';
import {useWindowSize} from '../state/hooks';

const itemRowStyle = css(textTheme, {
  marginLeft: "80px",
  paddingRight: "140px",
  paddingBottom: "30px",
  marginBottom: "50px",
  borderLeft: "10px solid LightCoral",
});

const collapsedItemRowStyle = css(textTheme, {
  marginLeft: "30px",
  fontSize: "0.8em",
  paddingRight: "50px",
  paddingBottom: "30px",
  marginBottom: "50px",
  borderLeft: "10px solid LightCoral",
})

function ItemRow(props: {
  markdown: Markdown
}) {
  const {markdown} = props;
  const {header} = markdown;
  const {id, title, tag, time} = header;
  const {width} = useWindowSize();

  return (
    <Container {...(width > 600 ? itemRowStyle : collapsedItemRowStyle)}>
      <Col>
        <h2 style={{fontSize: "1.5em"}}>
          <HoverLink text={title}
            link={`${process.env.PUBLIC_URL}/#/article/${id}`}
            ogColor={"DimGrey"}
            onHoverColor={"LightCoral"} />
        </h2>
      </Col>
      <Col {...css({fontSize: "1em", paddingLeft: "20px", color: "LightCoral", fontWeight: "bold"})}>
        {time.toJSON().replace(/-/gi, '.').split('T')[0]}
      </Col>
      <Col {...css({paddingLeft: "22px"})}>
        <h4>
          {
            tag?.map(t => (
              <span key={t}>
                <Badge variant="light">
                  <Link to={`/tag/${t}`} style={{...linkStyle, color: "LightCoral"}}>
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
  const lists =
    markdowns.map(m => <ItemRow markdown={m} key={m.header.id} />);
  return (
    <Container>
      {
        lists
      }
    </Container>
  );
}

export function ArticleChronoList() {
  return <List markdowns={chronoList(articlesDB)} />
}

export function NotesChronoList() {
  return <List markdowns={chronoList(notesDB)} />
}

export function OthersChronoList() {
  return <List markdowns={chronoList(othersDB)} />
}

export function ReportChronoList() {
  return <List markdowns={chronoList(othersDB)} />
}

export function TagList() {
  const {tag} = useParams();
  return <List markdowns={allDB.get(tag as string) ?? []} />
}
