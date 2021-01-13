import React, {useState, useRef, CSSProperties} from 'react';
import {Row, Col, Container} from 'react-bootstrap';
import {FaBars, FaTimes} from 'react-icons/fa';
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
    <span className={"header-title-font"}>
      <h2
        style={{letterSpacing: letterSpacing ?? "0.01em", }}>
        <b>
        jimmy yao
        </b>
    </h2>
    </span>
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
  const toggleTopPadding = width > 250 ? "100px" : "40px";
  return (
    <div style={{marginBottom: 40, color: "LightCoral"}}>

      <Container>
        <Background img={Beach} height={"10em"} />
        <div className={"header-title"}>
          <Title letterSpacing={"0em"} />
        </div>
        <div style={{paddingTop: toggleTopPadding, }}>
          <Toggle />
        </div>
      </Container>
    </div >
  )
}

export function Header() {
  const HeaderTitle = () => (
    <div className={"header-title"}>
      <Row>
        <Col>
          <Link to={'/'} style={{
            ...linkStyle,
            color: "LightCoral",
          }}>
            <Title />
          </Link>
        </Col>
        <Toggle />
      </Row>
    </div>
  );
  return (
    <div className={"header-main"}>
      <div>
        <Background img={Beach} />
        <HeaderTitle />
        <Avatar />
      </div>
    </div>
  )
}

// <Row style={{color: "WhiteSmoke"}}>
//   <b> ⊢ Email: jimmy123good@hotmail.com </b>
// </Row>

function Avatar() {
  const AvatarInfo = () => (
    <div className={"header-avatar-info"}>
      <Col >
        <Row>
          <HoverLink text={"⊢ Articles"}
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
        <Row>
          <HoverLink text={"⊢ Twitter"}
            link={"https://twitter.com/ailrk123"}
            ogColor={"WhiteSmoke"}
            onHoverColor={"LightCoral"} />
        </Row>
      </Col>
    </div>
  );
  return (
    <div className={"header-avatar"}>
      <Row>
        <Col xs={2}>
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

function Toggle() {
  const [sidebarOn, setSidebarOn] = useState<boolean>(false);
  const sidebarRef = useRef<HTMLDivElement>(null);

  const SidebarButton = () => (
    <FaBars size={35}
      className={"header-toggle-button"}
      onClick={() => {
        setSidebarOn(on => !on);
        const current = sidebarRef.current;
        current?.classList.remove("header-sidebar-slideout");
        current?.classList.toggle("header-sidebar-slidein");
        console.log(current?.classList);
      }} />
  );
  const CloseSideBarButton = () => (
    <FaTimes size={35}
      className={"header-toggle-button header-toggle-cross"}
      onClick={
        () => {
          setSidebarOn(on => !on);
          const current = sidebarRef.current;
          current?.classList.remove("header-sidebar-slidein");
          current?.classList.toggle("header-sidebar-slideout");
        }
      } />
  );

  return (
    <div className="header-toggle">
      {sidebarOn ? <CloseSideBarButton /> : <SidebarButton />}
      <Sidebar ref={sidebarRef} />
    </div>
  );
}
