import React from 'react';
import {
  TagList,
} from './List';
import {About} from './About';
import {AddPageTitle} from './Misc';
import "./Page.css";
import {allDB} from '../state/markdowns';
import {useParams} from 'react-router-dom';
import {Container, Badge} from 'react-bootstrap';
import {Link} from 'react-router-dom';
import {linkStyle} from '../styles/styleElements';


export const TagListPage: React.FC<{}> = () => {
  const {tag} = useParams();
  return <AddPageTitle pageTitle={`Tag: ${tag as string}`} page={<TagList />} />;
}

// export const PaperPage = Paper;
export const AboutMePage = About;


export function Tags() {
  const tags = Array.from(allDB.keys("tag")!);

  const TagLink = (props: {tag: string}) => (
    <Link to={`/tag/${props.tag}`}
      style={{...linkStyle, color: "LightCoral"}}>
      {props.tag}
    </Link>
  );

  const Tag = (props: {tag: string, idx: number}) => (
    <Badge
      variant="light"
      style={{marginRight: 10}}
      className={"page-tags-tag"}
      key={props.idx}>
      <TagLink tag={props.tag} />
    </Badge>);

  return (
    <Container>
      <div className={"page-tags-main"}
        style={{...linkStyle, }}>
        {
          tags.map((t, idx) => <Tag tag={t} idx={idx} />)
        }
      </div>
    </Container>);
}

export function TagPage() {
  return <AddPageTitle pageTitle={"Tag"} page={<Tags />} />
}
