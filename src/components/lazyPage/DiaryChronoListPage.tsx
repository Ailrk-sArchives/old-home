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

function int2ChineseDigit(n: number): string {
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

function int2ChineseNumTag(n: number): string {
  switch (n) {
    case 1: return "十";
    case 2: return "百";
    case 3: return "千";
    case 4: return "万";
    default: return "";
  }
}

function Let<T, R>(v: T, In: (v: T) => R): R {
  return In(v);
}

function int2ChineseNumber(n: number) {
  return Let(n.toString().split(""), (ns: string[]) => (
    Let(ns.length - 1, (len: number) => (
      ns.map((n, idx) => (
        Let(len - idx, (idx1: number) =>
          // supports two digits only.
          // 10
          // 01
          // 10
          (n == "1" && len == 1 && idx1 == 1
            || n == "0" && len == 1 && idx1 == 0
            ? ""
            : int2ChineseDigit(Number.parseInt(n)))
          + int2ChineseNumTag(idx1))))))
  )).join("");
}

function toChineseYear(time: Date) {
  return time
    .getFullYear()
    .toString()
    .split("")
    .map((n => int2ChineseDigit(Number.parseInt(n))))
    .concat(["年"])
    .join("");
}


function monthLength(n: number) {
  switch (n) {
    case 1:
    case 3:
    case 5:
    case 7:
    case 8:
    case 10:
    case 12:
      return 31;
    case 2:
      return 28;
    default:
      if (n > 12 || n < 1) {
        console.error("Invalid month");
        return 1;
      } else {
        return 30;
      }
  }
}

function toChineseDate(time: Date) {
  const year = time.getFullYear() + "年";
  const month = `${int2ChineseNumber(time.getMonth() + 1)}月`;
  const day = `${int2ChineseNumber(time.getDate())}日`;

  console.log(time);

  //
  // [0, 11]

  return `${year}${month} ${day}`
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

          // NOTE: javascript will read 2021-09-01 as 2021-08-31,
          // very intereting :(.
          // you need to remember convert it back by add 1.
          let time1 = new Date(time);
          time1.setDate(time1.getDate() + 1);
          const date = toChineseDate(time1);

          return (
            //  「${date}」 ﹁${date}﹂
            <HoverLink text={`  「${date}」 ` + title}
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
