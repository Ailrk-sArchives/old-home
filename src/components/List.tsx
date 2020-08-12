import {Markdown, chronoList, articlesDB, notesDB, othersDB, allDB} from '../state/markdowns';
import {Container, Row, Col, Badge} from 'react-bootstrap';
import React from 'react';
import {css} from 'glamor';
import {textTheme, linkStyle} from '../styles/styleElements';
import {Link, useParams} from 'react-router-dom';
import {HoverLink, toBoldH2} from './Misc';
import {useWindowSize} from '../state/hooks';
import {FaCaretRight} from 'react-icons/fa';

const itemRowStyle = css(textTheme, {
  marginLeft: "2em",
  paddingBottom: "1em",
  marginBottom: "1em",
});

const collapsedItemRowStyle = css(textTheme, {
  fontSize: "0.8em",
  paddingBottom: "30px",
  marginBottom: "50px",
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
        <h3 style={{fontSize: "1.5em"}}>

          <Row>
            <FaCaretRight
              display={"inline"}
              style={{
                color: "LightCoral",
                paddingTop: "0.2em",
              }} />
            <HoverLink text={title}
              link={`${process.env.PUBLIC_URL}/#/article/${id}`}
              ogColor={"DimGrey"}
              onHoverColor={"LightCoral"}
              element={toBoldH2} />
          </Row>
        </h3>
      </Col>
      <Col {...css({fontSize: "0.8em", paddingLeft: "20px", color: "Gainsboro", fontWeight: "bold"})}>
        {time.toJSON().replace(/-/gi, '.').split('T')[0]}
      </Col>
      <Col {...css({paddingLeft: "20px"})}>
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
