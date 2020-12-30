import React, {useState, CSSProperties} from 'react';
import {Container} from 'react-bootstrap';
import {linkStyle} from '../styles/styleElements';
import {useWindowSize, useDelayRender} from '../state/hooks';
import Loader from 'react-loader-spinner';
import "./Misc.css";

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
      <b className={"misc-to-bold-h2"}>
        <span style={{color: "LightCoral"}}>∘</span>
          &nbsp;
        {text}
      </b>
    </h2>);
}

export function Bar(props: {style?: CSSProperties}) {
  return (
    <div className={"misc-bar"} style={props?.style} />
  );
}

export function AddPageTitle(props: {pageTitle: string, page: JSX.Element}) {
  const {pageTitle, page} = props;
  const {width} = useWindowSize();
  let delay = useDelayRender();
  const AddPageTitleH2 = () => (
    <h2 className={"misc-add-page-title-h2"}
      style={{
        marginLeft: (width > 600 ? '3em' : '1.8em'),
      }}>{`${pageTitle}`}</h2>
  );
  const Content = () => (<> <AddPageTitleH2 /> {page} </>);
  return (
    <Container>
      {
        delay ? <Content /> : <div />
      }
    </Container>
  );
}

export function LoaderSpinner() {
  return <Loader type="TailSpin" color="LightCoral" height={100} width={100} />
}
