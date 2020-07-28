import React, {useState} from 'react';
import {Row, Col, Container} from 'react-bootstrap';
import {css, link} from 'glamor';
import {FaBeer, FaBars} from 'react-icons/fa';
import {Link} from 'react-router-dom';
import {linkStyle} from '../styles/styleElements';
import {Sidebar} from './Sidebar';
import Chiruno from '../assets/ghead.png';
// import SadWopper from '../assets/sadwooper.png';
// import Wubo from '../assets/wubo.jpg';
// import PoppeenImg from '../assets/popeen.jpg';

const headerStyle = css({
  height: "100px",
  width: "100%",
  paddingTop: "28px",
  paddingRight: "40px",
  paddingLeft: "20px",
  marginBottom: "30px",
});

export function Header() {
  return (
    <Container>
      <Row {...headerStyle} xs={8}>
        <Col>
          <Link to={'/'} style={{...linkStyle, color: "LightCoral"}}>
            <h1 style={{fontWeight: "bolder", textShadow: "0px 1px, 1px 0px, 1px 1px"}}>
              <FaBeer size={45} /><b> ⟨ A Bag of Words | ⚀ | ⚁ | ⚂ | ⚃ | ⚄ |  ⚅ ⟩ </b>
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
            border: "1px solid SlateGray",
            borderRadius: 90
          }} />
      </Col>
      <Col {...css({paddingTop: "30px"})}>
        <Row>
          <a href="https://ailrk.github.io/home" style={linkStyle}>
            Jimmy Yao's blog
          </a>
        </Row>
        <Row>
          <a href="https://github.com/ailrk" style={linkStyle}>
            github: https://github.com/ailrk
          </a>
        </Row>
        <Row> email: jimmy123good@hotmail.com </Row>
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
