import React from 'react';
import {Container, Row} from 'react-bootstrap';
import {css} from 'glamor';
import {Link} from 'react-router-dom';
import {linkStyle} from '../styles/styleElements';


const sidebarStyle = css({
  position: "fixed",
  height: "100%",
  width: "100%",
  background: "WhiteSmoke",
  paddingTop: "30px",
  paddingLeft: "30px",
  top: 0,
});

export function Sidebar(props: {
  setSidebarOn: React.Dispatch<React.SetStateAction<boolean>>
}) {
  return (
    <Container {...sidebarStyle}>
      <Tab name={"tags"} link={"tags"} />
      <Tab name={"tags"} link={"tags"} />
    </Container>
  );
}

const tabStyle = css({
  marginTop: "20px",
  marginLeft: "20px",
  marginBottom: "20px",
  fontSize: 20,
});

function Tab(props: {name: string, link: string}) {
  const {name, link} = props;
  return (
    <Row {...tabStyle}>
      <Link to={link} style={{...linkStyle, fontFamily: "monospace", color: "SlateGray"}}>
        {
          name
        }
      </Link>
    </Row>
  );
}
