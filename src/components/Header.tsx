import React, {useState, CSSProperties} from 'react';
import {Row, Col, Container} from 'react-bootstrap';
import {css} from 'glamor';
import {FaBars} from 'react-icons/fa';
import {Link} from 'react-router-dom';
import {linkStyle} from '../styles/styleElements';
import {Sidebar} from './Sidebar'
import {HoverLink} from './Misc';
import {useWindowSize} from '../state/hooks';
import Chiruno from '../assets/ghead.png';
import Beach from '../assets/planet.jpg';
import "./Header.css";
import {CSSTransition} from 'react-transition-group';

function Title(props: {letterSpacing?: string}) {
  const {letterSpacing} = props;
  return (
    <h1 className={"header-title-font"}
      style={{letterSpacing: letterSpacing ?? "0.01em", }}>
      A Bag of Words
    </h1>
  );
}

function Background(props: {img: string, height?: string}) {
  const {img, height} = props;
  const {width} = useWindowSize();
  return (
    <>
      <div className={"header-background"} style={{
        backgroundImage: `url(${img})`,
        height: height ?? '23em',
      }}
      />
    </>
  );
}

export function CollapsedHeader() {
  const {width} = useWindowSize();
  const toggleTopPadding = width > 340 ? "100px" : "50px";
  return (
    <div style={{marginBottom: 40, color: "LightCoral"}}>

      <Container>
        <Background img={Beach} height={"10em"} />
        <div className={"collapsed-header-title"}>
          <Title letterSpacing={"0em"} />
        </div>
        <div className={"collapsed-header-toggle"}
          style={{paddingTop: toggleTopPadding, }}>
          <Toggle />
        </div>
      </Container>
    </div >
  )
}

export function Header() {
  const HeaderTitle = () => (
    <div className={"header-title"}>
      <Row xs={8}>
        <Col>
          <Link to={'/'} style={{
            ...linkStyle,
            color: "LightCoral",
          }}>
            <Title />
          </Link>
        </Col>
        <Row> <Toggle /> </Row>
      </Row>
    </div>
  );
  return (
    <div className={"header-main"}>
      <Background img={Beach} />
      <Container>
        <HeaderTitle />
        <Row>
          <Avatar />
        </Row>
      </Container>
    </div>
  )
}

function Avatar() {
  const AvatarInfo = () => (
    <div className={"header-avatar-info"}>
      <Col >
        <Row>
          <HoverLink text={"⊢ Jimmy Yao's blog"}
            link={"https://ailrk.github.io/home"}
            ogColor={"WhiteSmoke"}
            onHoverColor={"LightCoral"} />
        </Row>
        <Row>
          <HoverLink text={"⊢ Github"}
            link={"https://github.com/ailrk"}
            ogColor={"WhiteSmoke"}
            onHoverColor={"LightCoral"} />
        </Row>
        <Row style={{color: "WhiteSmoke"}}>
          <b> ⊢ Email: jimmy123good@hotmail.com </b>
        </Row>
      </Col>

    </div>
  );
  return (
    <div className={"header-avatar"}>
      <Row>
        <Col xs={3}>
          <img src={Chiruno}
            width={150}
            height={150}
            className={"header-avatar-img"} />
        </Col>
        <AvatarInfo />
      </Row>
    </div>
  );
}

function Toggle(props: {style?: CSSProperties}) {
  const {style} = props;
  const [sidebarOn, setSidebarOn] = useState<boolean>(false);
  const SidebarButton = () => (
    <FaBars size={35}
      className={"header-toggle"}
      style={style}
      onClick={() => {setSidebarOn(s => !s)}} />
  );

  return (
    <div>
      {!sidebarOn && <SidebarButton />}
      <CSSTransition
        in={sidebarOn}
        timeout={100}
        unmountOnExit>
        <div>
          <Sidebar setSidebarOn={setSidebarOn} />
        </div>
      </CSSTransition>
    </div>
  );
}
