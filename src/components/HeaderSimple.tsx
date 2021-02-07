import React, {useState} from 'react';
import {Row, Col, Container} from 'react-bootstrap';
import {Link} from 'react-router-dom';
import {linkStyle} from '../styles/styleElements';
import "./HeaderSimple.css";
import Ghead from '../assets/ghead.png';

function TopbarNavTab(props: {name: string, link: string}) {

  const [color, setColor] = useState<string>("SlateGray");
  return (
    <Col className="header-simple-info" sm={1}>
      <Link style={{...linkStyle, color}} to={props.link}
        onMouseEnter={() => setColor("lightCoral")}
        onMouseLeave={() => setColor("SlateGray")}>
        <h4>{props.name}</h4>
      </Link>
    </Col>);
}


export function HeaderSimple() {

  return (
    <div className="header-simple-main">
      <Container>
        <Row className="header-simple-text">
          <Col xl={8}>
            <h2 id="header-simple-title-text" >
              <span>
                <img src={Ghead}
                  alt="."
                  height={60}
                  width={60}
                  className="header-simple-avatar" />
              </span>

              ONZ, AILRK!
            </h2>
          </Col>

          <TopbarNavTab name="articles" link={"/"}/>
          <TopbarNavTab name="tags" link={"/tags"}/>
          <TopbarNavTab name="notes" link={"/notes"}/>
          <TopbarNavTab name="about" link={"/about"}/>
        </Row>
      </Container>
      <hr id="header-simple-separator-1" />
    </div>
  );
}
