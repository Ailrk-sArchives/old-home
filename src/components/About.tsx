import React from 'react';
import {Container} from 'react-bootstrap';
import {HoverLink} from './Misc';
import {useWindowSize} from '../state/hooks';
import {FaGithub, FaPaperPlane, FaTwitter, FaUserFriends} from "react-icons/fa/index";
import "./About.css";
import {Link} from 'react-router-dom';


let descrption = `
  I work on some haskell, c++, python and typescript.
`;

export function About() {
  const {width} = useWindowSize();
  return (
    <Container style={width > 600 ? {} : {marginLeft: 30}}>
      <div>
        <h5 className="about-title"> About me </h5>
      </div>
      <hr />

      <p className="about-descrption">{descrption}</p>
      <br />

      <HoverLink text={"Resume"}
        link={"https://github.com/ailrk/resume/blob/master/resume.pdf"}
        ogColor={"DimGray"}
        onHoverColor={"LightCoral"}
        element={e => (<h5 className="about-info"> <FaPaperPlane /> {e}</h5>)} />
      <HoverLink text={" Github"}
        link={"https://github.com/ailrk/"}
        ogColor={"DimGray"}
        onHoverColor={"LightCoral"}
        element={e => (<h5 className="about-info"> <FaGithub />{e}</h5>)} />
      <HoverLink text={" Twitter"}
        link={"https://twitter.com/ailrk123"}
        ogColor={"DimGray"}
        onHoverColor={"LightCoral"}
        element={e => (<h5 className="about-info"> <FaTwitter />{e}</h5>)} />

      <Link to={"/friends"}>
        <HoverLink text={" Friends"}
          link={"/friends"}
          ogColor={"DimGray"}
          onHoverColor={"LightCoral"}
          element={e => (<h5 className="about-info"> <FaUserFriends />{e}</h5>)} />
      </Link>
    </Container>
  );
}
