import React from 'react';
import {Container} from 'react-bootstrap';
import {HoverLink} from './Misc';
import {useWindowSize} from '../state/hooks';


let descrption = `
  I do some Haskell, c++, typescript, and python.
`;

export function About() {
  const {width} = useWindowSize();
  return (
    <Container style={width > 600 ? {} : {marginLeft: 30}}>
      <div style={{color: "LightCoral", }}>
        <h3> About me </h3>
      </div>
      <hr />

      <h4 style={{color: "DimGray", }}>{descrption}</h4>
      <br/>

      <HoverLink text={"→ My Resume"}
        link={"https://github.com/ailrk/resume/blob/master/resume.pdf"}
        ogColor={"DimGray"}
        onHoverColor={"LightCoral"}
        element={e => (<h5>{e}</h5>)} />
      <HoverLink text={"→ My Github"}
        link={"https://github.com/ailrk/"}
        ogColor={"DimGray"}
        onHoverColor={"LightCoral"}
        element={e => (<h5>{e}</h5>)} />
      <HoverLink text={"→ My Twitter"}
        link={"https://twitter.com/ailrk123"}
        ogColor={"DimGray"}
        onHoverColor={"LightCoral"}
        element={e => (<h5>{e}</h5>)} />

    </Container>
  );
}
