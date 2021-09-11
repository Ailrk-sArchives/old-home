import React from 'react';
import { useParams } from 'react-router-dom';

import { Container } from 'react-bootstrap';
import { AddPageTitle, HoverLink } from '../Misc';
import { FaLink } from "react-icons/fa/index";
import { useWindowSize } from '../../state/hooks';
import { useDelayRender } from '../..//state/hooks';
import { diaryDB, chronoLists } from '../../state/markdowns';


import "./DiaryChronoListPage.css";

// ArticleChronoListPage
export default () => {
  return <AddPageTitle pageTitle={"碎碎念"} page={<DiaryChronoList />} />;
}

function int2ChineseNumber(n: number): string {
  switch (n) {
    case 0: return "零";
    case 1: return "一";
    case 2: return "二";
    case 3: return "三";
    case 4: return "四";
    case 5: return "五";
    case 6: return "六";
    case 7: return "七";
    case 8: return "八";
    case 9: return "九";
    case 10: return "十";
    default:
      return "零";
  }
}
function toChineseYear(time: Date) {
  return time
    .getFullYear()
    .toString()
    .split("")
    .map((n => int2ChineseNumber(Number.parseInt(n))))
    .concat(["年"])
    .join("");
}

function toChineseDate(time: Date) {
  const year = time.getFullYear() + "  / 年";
  const day = `${int2ChineseNumber(time.getDate() + 1)}日`;
  const month = `${int2ChineseNumber(time.getMonth() + 1)}月`;

  return `${year} ${month} ${day}`
}

function DiaryChronoList() {
  const { width } = useWindowSize();
  let delay = useDelayRender();
  let diaries = chronoLists.diaryChronoLists;

  let Content = () => (
    <>
      {
        diaries.map(diary => {
          const {
            time,
            title,
            id,
          } = diary.header;
          const date = toChineseDate(time);

          return (
            //  「${date}」 ﹁${date}﹂
            <HoverLink text={`  ﹁${date}﹂ ` + title}
              link={`${process.env.PUBLIC_URL}/#/diary/${id}`}
              ogColor={"black"}
              onHoverColor={"LightCoral"}
              element={e => (<h5 className="diary-info">{e}</h5>)} />);
        })
      }
    </>
  );
  return (
    <Container className="diary-page" style={width > 600 ? {} : { marginLeft: 30 }}>
      {
        delay ? <Content /> : <div />
      }
    </Container>
  );
}
