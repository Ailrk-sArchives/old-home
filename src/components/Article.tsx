import React, {useEffect, useState, useRef} from 'react';
import {Container, Row} from 'react-bootstrap';
import {StyleAttribute, css} from 'glamor';
import {Markdown} from '../state/markdowns';
import {textTheme} from '../styles/styleElements';
import {HoverLink} from './Misc';
import '../styles/Article.css';

const defaultArticleStyle = css(textTheme, {
  paddingLeft: "101px",
  paddingTop: "30px",
  marginBottom: "100px",
  overflow: "hidden",
});

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

  // avoid asyc complete after component is unmounted.
  useEffect(() => {
    return () => {
      componentIsMounted.current = false;
    }
  })

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

  return (
    <Container {...style ?? defaultArticleStyle}>
      <div {...css({
        width: "90%",
        wordBreak: "break-word",
        marginBottom: "100px"
      })}>
        <div className="Article" dangerouslySetInnerHTML={{__html: article}} />
      </div>
      <hr />
      <h1 style={{color: "DimGray", fontWeight: "bolder"}}>Source</h1>
      <SourceList sources={markdown?.header.source} />
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
          onHoverColor={"LightCoral"} />
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
