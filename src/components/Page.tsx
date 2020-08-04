import React from 'react';
import {Article} from './Article';
import {ArticleChronoList, NotesChronoList, TagList} from './List';
import {allDB} from '../state/markdowns';
import {useParams} from 'react-router-dom';
import {Container, Badge} from 'react-bootstrap';
import {Link} from 'react-router-dom';
import {linkStyle} from '../styles/styleElements';

export function ArticlePage() {
  const {id} = useParams();
  const markdown = allDB.get(Number.parseInt(id as string));
  return <Article markdown={markdown} />;
}

export const ArticleChronoListPage: React.FC<{}> = () =>
  <AddPageTitle pageTitle={"Articles"} page={<ArticleChronoList />} />;

export const NoteChronoListPage: React.FC<{}> = () =>
  <AddPageTitle pageTitle={"Notes"} page={<NotesChronoList />} />;

export const TagListPage: React.FC<{}> = () => {
  const {tag} = useParams();
  return <AddPageTitle pageTitle={`Tag: ${tag as string}`} page={<TagList />} />;
}

export function TagsPage() {
  const tags = Array.from(allDB.keys("tag")!);
  return (
    <Container>
      <h3 style={{
        color: "DimGray",
        fontWeight: "bold",
          marginLeft: 30,
          marginBottom: 40,
      }}>All Tags</h3>
      <div style={{
        ...linkStyle,
          fontSize: 25,
          paddingLeft: 40,
          paddingRight: 100,
      }}>
        {
          tags.map(t =>
            <Badge variant="light" style={{marginRight: 10}}>
              <Link to={`/tag/${t}`} style={{...linkStyle, color: "LightCoral"}}>
                {t}
              </Link>
            </Badge>)
        }
      </div>
    </Container>);
}

function AddPageTitle(props: {pageTitle: string, page: JSX.Element}) {
  const {pageTitle, page} = props;
  return (
    <Container>
      <h2 style={{
        color: "DimGray",
        fontWeight: "bold",
        marginBottom: 40,
        marginLeft: 80,
      }}>{`${pageTitle}`}</h2>
      {
        page
      }
    </Container>
  );
}


