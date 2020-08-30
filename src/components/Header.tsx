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
import Beach from '../assets/beach.png';

const headerStyle = css({
  height: "100px",
  width: "100%",
  paddingTop: "28px",
  paddingRight: "40px",
  paddingLeft: "80px",
  marginBottom: "30px",
});

function Title(props: {letterSpacing?: string}) {
  const {letterSpacing} = props;
  return (
    <h1 style={{
      fontSize: "3em",
      letterSpacing: letterSpacing ?? "0.05em",
      fontFamily: "RobotoSlab",
    }}>
      A Bag of Words
    </h1>
  );
}

function Background(props: {img: string, height?: string}) {
  const {img, height} = props;
  return (

    <div style={{
      backgroundImage: `url(${img})`,
      backgroundRepeat: 'no-repeat',
      backgroundSize: 'cover',
      top: 0,
      bottom: 0,
      left: 0,
      right: 0,
      pointerEvents: 'none',
      filter: "contrast(30%)",
      height: height ?? '20em',
      width: '100%',
      position: 'absolute',
      overflow: 'hidden',
      zIndex: -1,
    }}
    />
  );
}

export function CollapsedHeader() {
  const {width} = useWindowSize();
  const toggleTopPadding = width > 700 ? "100px" : "30px";
  return (
    <div style={{marginBottom: 40, color: "WhiteSmoke"}}>

      <Container>
        <Background img={Beach} height={"10em"} />
        <Row style={{
          marginLeft: "20px",
          paddingRight: "50px",
          marginTop: "30px",
        }}> <Title letterSpacing={"0em"} /> </Row>

        <Row style={{
          paddingTop: toggleTopPadding,
          marginLeft: "10px",
          marginBottom: "15px",
        }}>
          <Toggle style={{color: "LightCoral"}} />
        </Row>
      </Container>
    </div >
  )
}

export function Header() {
  return (
    <div className={"Header"}
      style={{
        marginBottom: 80,
      }}>
      <Background img={Beach} />
      <Container>
        <Row {...headerStyle} xs={8}>
          <Col>
            <Link to={'/'} style={{
              ...linkStyle,
              color: "white",
            }}>
              <Title />
            </Link>
          </Col>
          <Row> <Toggle /> </Row>
        </Row>
        <Row>
          <Avatar />
        </Row>
      </Container>
    </div>
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
            border: "white solid 5px",
          }} />
      </Col>
      <Col {...css({paddingTop: "30px"})}>
        <Row>
          <HoverLink text={"Jimmy Yao's blog"}
            link={"https://ailrk.github.io/home"}
            ogColor={"white"}
            onHoverColor={"LightCoral"} />
        </Row>
        <Row>
          <HoverLink text={"Github: https://github.com/ailrk"}
            link={"https://github.com/ailrk"}
            ogColor={"white"}
            onHoverColor={"LightCoral"} />
        </Row>
        <Row {...css({color: "white"})}>
          <b> Email: jimmy123good@hotmail.com </b>
        </Row>
      </Col>
    </Row>
  );
}
function Toggle(props: {style?: CSSProperties}) {
  const {style} = props;
  const toggleStyle: CSSProperties = {
    ...{
      cursor: "pointer",
      paddingTop: "4px",
      color: "WhiteSmoke"
    },
    ...style,
  };

  const [sidebarOn, setSidebarOn] = useState<boolean>(false);
  return (
    <>
      {
        sidebarOn ? <Sidebar setSidebarOn={setSidebarOn} /> :
          <FaBars size={35}
            style={toggleStyle}
            onClick={() => {setSidebarOn(s => !s)}} />
      }
    </>);
}
