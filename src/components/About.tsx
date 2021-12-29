import React from 'react';
import { Container } from 'react-bootstrap';
import { HoverLink } from './Misc';
import { useWindowSize } from '../state/hooks';
import { FaGithub, FaPaperPlane, FaTwitter, FaUserFriends, FaLink } from "react-icons/fa/index";
import "./About.css";
import { Link } from 'react-router-dom';
import { useDelayRender } from '../state/hooks';


let descrption = `
  I like everything about programming languages.
`;

export function About() {
  const { width } = useWindowSize();
  let delay = useDelayRender();
  const Content = () => (
    <>
      <div>
        <h5 className="about-title"> About me </h5>
      </div>
      <hr />

      <p className="about-descrption">{descrption}</p>
      <br />

      <HoverLink text={" Github"}
        link={"https://github.com/ailrk/"}
        ogColor={"black"}
        onHoverColor={"LightCoral"}
        element={e => (<h5 className="about-info"> <FaGithub />{e}</h5>)} />
      <HoverLink text={" Twitter"}
        link={"https://twitter.com/ailrk123"}
        ogColor={"black"}
        onHoverColor={"LightCoral"}
        element={e => (<h5 className="about-info"> <FaTwitter />{e}</h5>)} />
      <HoverLink text={"Resume"}
        link={"https://github.com/ailrk/resume/blob/master/resume.pdf"}
        ogColor={"black"}
        onHoverColor={"LightCoral"}
        element={e => (<h5 className="about-info"> <FaPaperPlane /> {e}</h5>)} />
      <Link to={"/friends"}>
        <HoverLink text={" Friends"}
          link={"/friends"}
          ogColor={"black"}
          onHoverColor={"LightCoral"}
          element={e => (<h5 className="about-info"> <FaUserFriends />{e}</h5>)} />
      </Link>

      <Link to={"/wiki"}>
        <HoverLink text={"Wiki"}
          link={"/diary"}
          ogColor={"black"}
          onHoverColor={"LightCoral"}
          element={e => (<h5 className="about-info"> <FaUserFriends />{e}</h5>)} />
      </Link>
    </>
  );

  return (
    <Container className="about-body" style={width > 600 ? {} : { marginLeft: 30 }}>
      {
        delay ? <Content /> : <div />
      }
    </Container>
  );
}
