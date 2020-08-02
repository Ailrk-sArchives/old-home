import React, {useEffect, useState, useRef} from 'react';
import {Container} from 'react-bootstrap';
import {StyleAttribute, css} from 'glamor';
import {Markdown, articlesDB} from '../state/markdowns';
import {textTheme} from '../styles/styleElements';
import {useParams} from 'react-router-dom';
import '../styles/Article.css';

export function ArticlePage() {
  const {id} = useParams();
  const markdown = articlesDB.get(Number.parseInt(id as string));
  return <Article markdown={markdown} />;
}

const defaultArticleStyle = css(textTheme, {
  paddingLeft: "101px",
  paddingTop: "30px",
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
      <div {...css({width: "90%", wordBreak: "break-word"})}>
        <div className="Article" dangerouslySetInnerHTML={{__html: article}} />
      </div>
    </Container>
  );
}
