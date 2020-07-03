import React, {useState} from 'react';
import {Row, Col, Container} from 'react-bootstrap';
import {css} from 'glamor';
import {FaBeer, FaBars} from 'react-icons/fa';
import {Link} from 'react-router-dom';
import {linkStyle} from '../styles/styleElements';
import {Sidebar} from './Sidebar';

const headerStyle = css({
  height: "100px",
  width: "100%",
  paddingTop: "28px",
  paddingRight: "40px",
  paddingLeft: "20px",
  marginBottom: "130px",
});

export function Header() {
  return (
    <Container>
      <Row {...headerStyle} xs={8}>
        <Col>
          <Link to={'/home'} style={linkStyle}>
            <h1>
              <FaBeer size={45} /><b>&nbsp; A Bag of Words </b>
            </h1>
          </Link>
        </Col>
        <Row>
          <Toggle />
        </Row>
      </Row>
      <hr {...css({paddingBottom: "30px"})} />
    </Container>
  )
}

function Toggle() {
  const toggleStyle = css({
    cursor: "pointer",
  });
  const [sidebarOn, setSidebarOn] = useState<boolean>(false);
  return (
    <>
      {
        sidebarOn ? <Sidebar setSidebarOn={setSidebarOn} /> :
          <FaBars size={35} {...toggleStyle} onClick={() => {setSidebarOn(s => !s)}} />
      }
    </>);
}
