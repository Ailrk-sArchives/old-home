import React from 'react';
import {Container} from 'react-bootstrap';
import {StyleAttribute, css} from 'glamor';
import {Markdown, db} from '../state/markdowns';
import {textTheme} from '../styles/styleElements';
import {useParams} from 'react-router-dom';
import '../styles/Article.css';

export function ArticlePage() {
  const {id} = useParams();
  const markdown = db.get(Number.parseInt(id as string));
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
  const article = markdown?.content ?? "Oppsy Doopsy!";
  console.log(article);

  return (
    <Container {...style ?? defaultArticleStyle}>
      <div {...css({width: "90%", wordBreak: "break-word"})}>
        <div className="Article" dangerouslySetInnerHTML={{__html: article}} />
      </div>
    </Container>
  );
}
