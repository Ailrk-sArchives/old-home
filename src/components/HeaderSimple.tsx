import React, {useState} from 'react';
import {Row, Col, Container} from 'react-bootstrap';
import {Link} from 'react-router-dom';
import {linkStyle} from '../styles/styleElements';
import "./HeaderSimple.css";
import Ghead from '../assets/ghead.png';

function TopbarNavTab(props: {name: string, link: string}) {

  const [color, setColor] = useState<string>("black");
  return (
    <div className="header-simple-info">
      <Link style={{...linkStyle, color}} to={props.link}
        onMouseEnter={() => setColor("lightCoral")}
        onMouseLeave={() => setColor("black")}>
        <span>{props.name}</span>
      </Link>
    </div>);
}


export function HeaderSimple() {

  return (
    <Container className="header-simple-main">
      <Row className="header-simple-text">
        <Col sm={8}>
          <Link style={{...linkStyle}} to="/">
            <span id="header-simple-title" >
              <span>
                <img src={Ghead}
                  alt="."
                  height={60}
                  width={60}
                  className="header-simple-avatar" />
              </span>
              <span id="header-simple-title-text"> 4 letters prose </span>
            </span>
          </Link>
        </Col>

        <Row className={"header-simple-navbar"}>
          <TopbarNavTab name="articles" link={"/"} />
          <TopbarNavTab name="tags" link={"/tags"} />
          <TopbarNavTab name="about" link={"/about"} />
        </Row>
      </Row>
    </Container>
  );
}
//<hr id="header-simple-separator-1" />
