import {css} from 'glamor';
import {CSSProperties} from 'react';

export const centerLabel = css({
  display: "flex",
  alignItems: "center",
});

export const textTheme = css({
  fontFamily: 'georgia, Times New Roman, Serif, DejaVu Serif',
  fontSize: "1.2em",
  lineHeight: 2.1,
});

export const leftAlign = css({
  paddingTop: "10px",
  paddingLeft: "30px",
});

export const linkStyle: CSSProperties = {
  textDecoration: 'none',
  color: 'black',
};
