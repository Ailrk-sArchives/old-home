import React, {useState} from 'react';
import {linkStyle} from '../styles/styleElements';

export function HoverLink(props: {
  text: string,
  link: string,
  ogColor: string,
  onHoverColor: string,
  element?: (text: string) => JSX.Element,
}) {
  const {text, link, ogColor, onHoverColor, element} = props;
  const [color, setColor] = useState<string>(ogColor);
  const ele = element?.(text) ?? (<b>{text}</b>);
  return (
    <a href={link} style={{...linkStyle, color: color}}
      onMouseEnter={() => setColor(onHoverColor)}
      onMouseLeave={() => setColor(ogColor)}>
      {ele}
    </a>);
}

export function toBoldH2(text: string) {
  return (
    <h2>
      <b style={{textShadow: "1px 0px, 0px 1px"}}>
        {text}
      </b>
    </h2>);
}


