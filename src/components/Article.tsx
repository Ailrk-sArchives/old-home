import React from 'react';
import {Container} from 'react-bootstrap';
import {StyleAttribute, css} from 'glamor';
import {Markdown, db} from '../state/markdowns';
import {textTheme} from '../styles/styleElements';
import {useParams} from 'react-router-dom';

export const defaultArticleStyle = css(textTheme, {
  marginLeft: "250px",
  marginRight: "200px",
  paddingLeft: "101px",
  paddingTop: "30px",
  overflow: "hidden",
});

export function ArticlePage() {
  const {id} = useParams();
  const markdown = db.get(Number.parseInt(id as string));
  return <Article markdown={markdown}/>;
}

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
        <div dangerouslySetInnerHTML={{__html: article}} />
      </div>
    </Container>
  );
}
