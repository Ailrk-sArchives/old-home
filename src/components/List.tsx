import {Markdown} from '../state/markdowns';
import {Container, Col, Badge} from 'react-bootstrap';
import React from 'react';
import {css} from 'glamor';
import {textTheme, linkStyle} from '../styles/styleElements';
import {Link} from 'react-router-dom';

const listRowStyle = css(textTheme, {
  paddingBottom: "80px",
});


function ListRow(props: {
  markdown: Markdown
}) {
  const {markdown} = props;
  const {header} = markdown;
  const {id, title, tag, time} = header;

  return (
    <Container {...listRowStyle}>
      <Col style={{paddingBottom: "12px"}}>
        <Link to={`/article/${id}`} style={linkStyle}>
          <h2><b>{title}</b></h2>
        </Link>
      </Col>
      <Col {...css({fontSize: 20, paddingLeft: "20px"})}>
        {time.toJSON().replace(/-/gi, ' . ').split('T')[0]}
      </Col>
      <Col>
        <h4>
          {
            tag?.map(t => (
              <>

                <Badge variant="danger">{t}</Badge>
                    &nbsp;
                </>
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
  const lists = markdowns.map(m => <ListRow markdown={m} />);
  return (
    <Container>
      {
        lists
      }
    </Container>
  );
}

