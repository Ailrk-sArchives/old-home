import React from 'react';
import {
  TagList,
} from './List';
import {allDB} from '../state/markdowns';
import {useParams} from 'react-router-dom';
import {Paper} from './Paper';
import {Container, Badge} from 'react-bootstrap';
import {Link} from 'react-router-dom';
import {linkStyle} from '../styles/styleElements';
import {About} from './About';
import {AddPageTitle} from './Misc';

export const TagListPage: React.FC<{}> = () => {
  const {tag} = useParams();
  return <AddPageTitle pageTitle={`Tag: ${tag as string}`} page={<TagList />} />;
}

export const PaperPage = Paper;

export const AboutMePage = About;

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
          tags.map((t, idx) =>
            <Badge variant="light" style={{marginRight: 10}} key={idx}>
              <Link to={`/tag/${t}`} style={{...linkStyle, color: "LightCoral"}}>
                {t}
              </Link>
            </Badge>)
        }
      </div>
    </Container>);
}
