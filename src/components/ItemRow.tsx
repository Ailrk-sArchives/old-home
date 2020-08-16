import React from 'react';
import {Markdown, MarkdownHeader} from '../state/markdowns';
import {textTheme, linkStyle} from '../styles/styleElements';
import {HoverLink, toBoldH2} from './Misc';
import {useWindowSize} from '../state/hooks';
import {Container, Col, Badge} from 'react-bootstrap';

import {Link} from 'react-router-dom';
import {css} from 'glamor';

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


function ItemRowSkeleton(props: {titleElement: JSX.Element, dateElement: JSX.Element, tagElement: JSX.Element}) {
  const {width} = useWindowSize();
  const {titleElement, dateElement, tagElement} = props;

  return (
    <Container {...(width > 600 ? itemRowStyle : collapsedItemRowStyle)}>
      <Col> {titleElement}</Col>
      <Col> {dateElement}</Col>
      <Col> {tagElement}</Col>
    </Container>
  );
}

const title = (header: MarkdownHeader) => (
  <h3 style={{fontSize: "1.5em"}}>
    <HoverLink text={`${header.title}`}
      link={`${process.env.PUBLIC_URL}/#/article/${header.id}`}
      ogColor={"DimGrey"}
      onHoverColor={"LightCoral"}
      element={toBoldH2} />
  </h3>

);

const date = (header: MarkdownHeader) => (
  <div {...css({paddingLeft: "20px", color: "Grey"})}>
    {header.time.toJSON().replace(/-/gi, '.').split('T')[0]}
  </div>
);

const tag = (header: MarkdownHeader) => {

  const tagListElements = header.tag?.map(t => (
    <span key={t}>
      <Badge variant="light">
        <Link
          to={`/tag/${t}`}
          style={{...linkStyle, color: "LightCoral"}}>
          {t}
        </Link>
      </Badge>
                    &nbsp;
    </span>));
  return (
    <div {...css({paddingLeft: "20px"})}>
      <h4> {tagListElements} </h4>
    </div>
  );
}

export function ItemRow(props: {
  markdown: Markdown
}) {
  const {markdown} = props;
  const {header} = markdown;
  return ItemRowSkeleton({
    titleElement: title(header),
    dateElement: date(header),
    tagElement: tag(header)
  });
}
