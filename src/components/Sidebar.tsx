import React, {useState} from 'react';
import {Container, Row} from 'react-bootstrap';
import {css} from 'glamor';
import {Link} from 'react-router-dom';
import {linkStyle} from '../styles/styleElements';
import {FaTimes} from 'react-icons/fa';
import {useWindowSize} from '../state/hooks';


const wideSideBarStyle = css({
  top: 0,
  right: 0,
  position: "fixed",
  background: "WhiteSmoke",
  zIndex: 1000,
  paddingTop: "30px",
  height: "100%",
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
      <Tab name={"home"} link={"/"} />
      <Tab name={"tags"} link={"/tags"} />
      <Tab name={"notes"} link={"/notes"} />
      <Tab name={"others"} link={"/others"} />
      <Tab name={"paper"} link={"/paper"} />
      <Tab name={"about me"} link={"/about"} />
    </Container>
  );
}

const tabStyle = css({
  marginTop: "20px",
  marginLeft: "20px",
  marginBottom: "20px",
  fontSize: 20,
});

function Tab(props: {
  name: string, link: string
}) {
  const {name, link} = props;
  const [color, setColor] = useState<string>("SlateGray");
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
