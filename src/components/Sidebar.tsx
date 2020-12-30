import React, {useState, useEffect} from 'react';
import {Container, Row} from 'react-bootstrap';
import {css} from 'glamor';
import {Link, useParams, useLocation} from 'react-router-dom';
import {linkStyle} from '../styles/styleElements';
import {
  FaTimes, FaHome, FaTags, FaStickyNote, FaBook, FaHandPaper, FaMapPin
} from 'react-icons/fa';
import {useWindowSize} from '../state/hooks';
import "./SideBar.css";

export function Sidebar(props: {
  setSidebarOn: React.Dispatch<React.SetStateAction<boolean>>
}) {
  const {width} = useWindowSize();
  const {setSidebarOn} = props;
  const sidebarStyle = width > 1000 ?
        "sidebar-wide-sidebar-main" :
        "sidebar-collapsed-sidebar-main";
  return (
    <Container>
      <div className={sidebarStyle + " header-sidebar-slide"}>
        <FaTimes className={"sidebar-close-button"}
          onClick={() => setSidebarOn(on => !on)} />
        <Tab name={<><FaHome /> home</>} link={"/"} />
        <Tab name={<><FaStickyNote /> notes</>} link={"/notes"} />
        <Tab name={<><FaBook /> reports</>} link={"/reports"} />
        <Tab name={<><FaTags /> tags</>} link={"/tags"} />
        <Tab name={<><FaHandPaper /> paper</>} link={"/paper"} />
        <Tab name={<><FaMapPin /> about</>} link={"/about"} />
      </div>
    </Container>
  );
}

function Tab(props: {
  name: JSX.Element,
  link: string,
}) {
  const {name, link} = props;
  const [color, setColor] = useState<string>("SlateGray");
  const location = useLocation();

  return (
    <Row>
      <Link to={link}
        className={"sidebar-tab-main "}
        style={{...linkStyle, fontFamily: "monospace", color}}
        onMouseEnter={() => setColor("lightCoral")}
        onMouseLeave={() => setColor("SlateGray")}>
        {name}
      </Link>
    </Row>
  );
}
