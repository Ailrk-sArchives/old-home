import React, {useState, useEffect} from 'react';
import {Container, Row} from 'react-bootstrap';
import {css} from 'glamor';
import {Link, useParams, useLocation} from 'react-router-dom';
import {linkStyle} from '../styles/styleElements';
import {
  FaTimes, FaHome, FaTags, FaStickyNote, FaBook, FaHandPaper, FaMapPin
} from 'react-icons/fa';
import {useWindowSize} from '../state/hooks';
import {CSSTransition} from 'react-transition-group';


const wideSideBarStyle = css({
  top: 0,
  right: 0,
  position: "fixed",
  background: "WhiteSmoke",
  zIndex: 1000,
  transitionDuration: 250,
  paddingTop: "30px",
  height: "100%",
  fontSize: "2em",
  width: "20%",
});

const collapsedSidebarStyle = css({
  top: 0,
});

export function Sidebar(props: {
  setSidebarOn: React.Dispatch<React.SetStateAction<boolean>>
}) {
  const {width} = useWindowSize();
  const {setSidebarOn} = props;
  return (
    <Container {...(width > 1000 ? wideSideBarStyle : collapsedSidebarStyle)}>
      <FaTimes style={{
        marginLeft: "10px",
        color: "Salmon",
        cursor: "pointer",

        marginBottom: "30px",
      }}
        onClick={() => setSidebarOn(on => !on)} />
      <Tab name={<><FaHome /> home</>} link={"/"} />
      <Tab name={<><FaTags /> tags</>} link={"/tags"} />
      <Tab name={<><FaStickyNote /> notes</>} link={"/notes"} />
      <Tab name={<><FaBook /> reports</>} link={"/reports"} />
      <Tab name={<><FaHandPaper /> paper</>} link={"/paper"} />
      <Tab name={<><FaMapPin /> about</>} link={"/about"} />
    </Container>
  );
}

const tabStyle = css({
  marginTop: "20px",
  marginLeft: "20px",
  marginBottom: "20px",
  fontSize: "1em",
});

function Tab(props: {
  name: JSX.Element,
  link: string,
}) {
  const {name, link} = props;
  const [color, setColor] = useState<string>("SlateGray");
  const location = useLocation();

  return (
    <Row {...tabStyle}>
      <Link to={link}
        style={{...linkStyle, fontFamily: "monospace", color}}
        onMouseEnter={() => setColor("lightCoral")}
        onMouseLeave={() => setColor("SlateGray")}>
        {name}
      </Link>
    </Row>
  );
}
