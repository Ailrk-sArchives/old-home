import { Container } from 'react-bootstrap';
import { HoverLink } from './Misc';
import { FaUserFriends } from "react-icons/fa/index";
import React from 'react';
import "./About.css"; // reuse about's style
import { useWindowSize } from '../state/hooks';
import { useDelayRender } from '../state/hooks';

const friendLinks = [
  ["Lucas Pozza", "https://stillwwater.github.io/#/"]
];

export function Friends() {
  const { width } = useWindowSize();
  let delay = useDelayRender();
  let Content = () => (
    <>
      <div>
        <h5>Friends</h5>
      </div>
      {
        friendLinks.map(pair => {
          const [name, link] = pair;

          return (
            <HoverLink text={"  " + name}
              link={link}
              ogColor={"black"}
              onHoverColor={"LightCoral"}
              element={e => (<h5 className="about-info"> <FaUserFriends />{e}</h5>)} />);
        })
      }
    </>);
  return (
    <Container style={width > 600 ? {} : { marginLeft: 30 }}>
      {
        delay ? <Container /> : <div />
      }
    </Container>
  );
}
