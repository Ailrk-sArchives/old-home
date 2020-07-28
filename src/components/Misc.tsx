import React, {useState} from 'react';
import {linkStyle} from '../styles/styleElements';

export function HoverLink(props: {
  text: string,
  link: string,
  ogColor: string,
  onHoverColor: string,
}) {
  const {text, link, ogColor, onHoverColor} = props;
  const [color, setColor] = useState<string>(ogColor);
  return (
    <a href={link} style={{...linkStyle, color: color}}
      onMouseEnter={() => setColor(onHoverColor)}
      onMouseLeave={() => setColor(ogColor)}>
      <b>  {text}</b>
    </a>);
}


