import React, { useEffect, useState, useRef } from 'react';
import { Container, Row } from 'react-bootstrap';
import { StyleAttribute, css } from 'glamor';
import { Markdown } from '../state/markdowns';
import { textTheme } from '../styles/styleElements';
import { HoverLink, toNormalP } from './Misc';
import { useWindowSize, useDelayRender } from '../state/hooks';
import '../styles/Article.css';

// switch to mobile layout
const breakpoint = 1025;

// article compoenent. It insert parsed markown JSX element
// into card container.
// the style can be controlled by style parameter.
export function Article(props: {
  markdown?: Markdown,
  className?: string,
  style?: StyleAttribute,
}) {
  const { markdown, style } = props;
  const [article, setArticle] = useState<string>("");
  const componentIsMounted = useRef(true);
  const { width } = useWindowSize();
  const delay = useDelayRender(0.1);
  const defaultArticleStyle = css(textTheme, {
    paddingLeft: width > breakpoint ? "35px" : "20px",
    paddingTop: width > breakpoint ? "30px" : "0px",
    fontSize: width > breakpoint ? "1em" : "0.7em",
    marginBottom: "100px",
    overflow: "hidden",
  });

  // avoid asyc complete after component is unmounted.
  useEffect(() => {
    return () => {
      componentIsMounted.current = false;
    }
  }, []);

  useEffect(() => {
    const fetchArticle = async () => {
      try {
        // console.log(markdown?.content);
        const value = (await markdown?.content) ?? "Oppsy Doopsy!";
        if (componentIsMounted.current) {
          setArticle(value);
        }
      } catch (err) {
        console.error(err);
      }
    }
    fetchArticle();
  }, []);

  let c = "Article";
  if (props.className) {
    c += " " + props.className;
  }

  const Content = () => (
    <div>
      <div {...css({
        width: "90%",
        wordBreak: "break-word",
        marginBottom: "100px"
      })}>
        <div
          className={c} dangerouslySetInnerHTML={{ __html: article }} />
      </div>
      <hr />
      <h3 style={
        {
          color: "black",
          fontWeight: "bolder"
        }}>Source</h3>
      <SourceList sources={markdown?.header.source} />
    </div>
  );

  return (
    <Container fluid="lg" className="page-body" {...style ?? defaultArticleStyle} >
      {
        delay ? <Content /> : <div />
      }
    </Container>
  );
}

function SourceList(props: { sources?: Array<string> }) {
  const { sources } = props;
  const row = (source: string, idx: number) => {
    return (
      <Row key={idx}>
        <HoverLink text={source}
          link={source}
          ogColor={"black"}
          onHoverColor={"LightCoral"}
          element={(s: string) => (
            <span className="source-list" >
              {s}
            </span>)
          }
        />
      </Row>
    )
  };
  return (
    <Container>
      {
        sources?.map((s, idx) => row(s, idx))
      }
    </Container>
  )
}
