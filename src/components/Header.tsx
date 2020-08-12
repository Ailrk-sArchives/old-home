import React, {useState} from 'react';
import {Row, Col, Container} from 'react-bootstrap';
import {css} from 'glamor';
import {FaBars} from 'react-icons/fa';
import {Link} from 'react-router-dom';
import {linkStyle} from '../styles/styleElements';
import {Sidebar} from './Sidebar'
import {HoverLink} from './Misc';
import Chiruno from '../assets/ghead.png';

const headerStyle = css({
  height: "100px",
  width: "100%",
  paddingTop: "28px",
  paddingRight: "40px",
  paddingLeft: "80px",
  marginBottom: "30px",
});

export function CollapsedHeader() {
  return (
    <Container>
      <Row style={{paddingTop: "40px", marginLeft: "30px"}}>
      <Toggle />
      </Row>
      <hr {...css({paddingBottom: "30px", marginTop: "50px"})} />
    </Container>
  )
}

export function Header() {
  return (
    <Container>
      <Row {...headerStyle} xs={8}>
        <Col>
          <Link to={'/'} style={{...linkStyle, color: "LightCoral"}}>
            <h1 style={{fontSize: "2em", fontWeight: "bolder", textShadow: "0px 1px, 1px 0px, 1px 1px"}}>
              <b> A Bag of Words  </b>
            </h1>
          </Link>
        </Col>
        <Row>
          <Toggle />
        </Row>
      </Row>
      <Row>
        <Avatar />
      </Row>
      <hr {...css({paddingBottom: "30px", marginTop: "50px"})} />
    </Container>
  )
}

const avatarStyle = css({
  paddingLeft: "100px",
  width: "100%",
});

function Avatar() {
  return (
    <Row {...avatarStyle}>
      <Col xs={3}>
        <img src={Chiruno}
          width={150}
          height={150}
          style={{
            borderRadius: 90,
          }} />
      </Col>
      <Col {...css({paddingTop: "30px"})}>
        <Row>
          <HoverLink text={"Jimmy Yao's blog"}
            link={"https://ailrk.github.io/home"}
            ogColor={"DimGray"}
            onHoverColor={"LightCoral"} />
        </Row>
        <Row>
          <HoverLink text={"Github: https://github.com/ailrk"}
            link={"https://github.com/ailrk"}
            ogColor={"DimGray"}
            onHoverColor={"LightCoral"} />
        </Row>
        <Row {...css({color: "DimGray"})}>
          <b> Email: jimmy123good@hotmail.com </b>
        </Row>
      </Col>
    </Row>
  );
}

function Toggle() {
  const toggleStyle = css({
    cursor: "pointer",
    paddingTop: "4px",
  });
  const [sidebarOn, setSidebarOn] = useState<boolean>(false);
  return (
    <>
      {
        sidebarOn ? <Sidebar setSidebarOn={setSidebarOn} /> :
          <FaBars size={35}
            {...toggleStyle}
            style={{color: "LightCoral"}}
            onClick={() => {setSidebarOn(s => !s)}} />
      }
    </>);
}
