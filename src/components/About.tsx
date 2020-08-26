import React from 'react';
import {Container} from 'react-bootstrap';
import {HoverLink} from './Misc';


export function About() {
  return (
    <Container>
      <div style={{color: "LightCoral", }}>
        <h1> About me </h1>
      </div>
      <hr />

      <HoverLink text={"→ My Resume"}
        link={"https://github.com/ailrk/resume/blob/master/resume.pdf"}
        ogColor={"DimGray"}
        onHoverColor={"LightCoral"}
        element={e => (<h3>{e}</h3>)} />
      <HoverLink text={"→ My Github"}
        link={"https://github.com/ailrk/"}
        ogColor={"DimGray"}
        onHoverColor={"LightCoral"}
        element={e => (<h3>{e}</h3>)} />

    </Container>
  );
}
