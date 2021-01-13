import React, {useState, useEffect} from 'react';
import {Container, Row} from 'react-bootstrap';
import {Link, useParams, useLocation} from 'react-router-dom';
import {linkStyle} from '../styles/styleElements';
import {
  FaHome, FaTags, FaStickyNote, FaBook, FaHandPaper, FaMapPin
} from 'react-icons/fa';
import "./SideBar.css";

export const Sidebar = React.forwardRef((props, ref) => {
  return (
    <div className={"sidebar-main"}
      ref={ref as React.RefObject<HTMLDivElement>}>
      <Tab name={<><FaHome /> home</>} link={"/"} />
      <Tab name={<><FaStickyNote /> notes</>} link={"/notes"} />
      <Tab name={<><FaTags /> tags</>} link={"/tags"} />
     <Tab name={<><FaMapPin /> about</>} link={"/about"} />
    </div>
  );
});

      // <Tab name={<><FaBook /> reports</>} link={"/reports"} />
      // <Tab name={<><FaHandPaper /> paper</>} link={"/paper"} />

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
        style={{...linkStyle, fontFamily: "Serif", color}}
        onMouseEnter={() => setColor("lightCoral")}
        onMouseLeave={() => setColor("SlateGray")}>
        {name}
      </Link>
    </Row>
  );
}
