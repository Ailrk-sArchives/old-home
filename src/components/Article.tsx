import React, {useEffect, useState, useRef} from 'react';
import {Container, Row} from 'react-bootstrap';
import {StyleAttribute, css} from 'glamor';
import {Markdown} from '../state/markdowns';
import {textTheme} from '../styles/styleElements';
import {HoverLink, toNormalP} from './Misc';
import {useWindowSize, useDelayRender} from '../state/hooks';
import '../styles/Article.css';

// article compoenent. It insert parsed markown JSX element
// into card container.
// the style can be controlled by style parameter.
export function Article(props: {
  markdown?: Markdown,
  style?: StyleAttribute,
}) {
  const {markdown, style} = props;
  const [article, setArticle] = useState<string>("");
  const componentIsMounted = useRef(true);
  const {width} = useWindowSize();
  const delay = useDelayRender(100);
  const defaultArticleStyle = css(textTheme, {
    paddingLeft: width > 1000 ? "60px" : "20px",
    paddingTop: width > 1000 ? "30px" : "0px",
    fontSize: width > 1000 ? "1em" : "0.7em",
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
        console.log(markdown?.content);
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

  const Content = () => (
    <>
      <div {...css({
        width: "90%",
        wordBreak: "break-word",
        marginBottom: "100px"
      })}>
        <div className="Article" dangerouslySetInnerHTML={{__html: article}} />
      </div>
      <hr />
      <h3 style={
        {
          color: "DimGray",
          fontWeight: "bolder"
        }}>Source</h3>
      <SourceList sources={markdown?.header.source} />
    </>
  );

  return (
    <Container fluid="lg" className="page-body" {...style ?? defaultArticleStyle} >
      {
        delay ? <Content /> : <div />
      }
    </Container>
  );
}

function SourceList(props: {sources?: Array<string>}) {
  const {sources} = props;
  const row = (source: string, idx: number) => {
    return (
      <Row key={idx}>
        <HoverLink text={source}
          link={source}
          ogColor={"DimGray"}
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
