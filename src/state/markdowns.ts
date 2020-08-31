import {
  MarkdownRuntimeDatabase,
  MarkdownStaticDatabase,
  AllDB,
} from 'markdowndb.macro/dist/markdown-map';

import markdowndb, {Markdown, MarkdownDB} from 'markdowndb.macro';
import {unique, flat} from '../untils/list-ops';

export const articlesDB: MarkdownDB = markdowndb({
  markdownDir: 'articles',
  mode: 'static',
  publicURL: "/home",
  logLevel: "silence",
});

export const notesDB: MarkdownDB = markdowndb({
  markdownDir: "notes",
  mode: "static",
  publicURL: "/home",
  logLevel: "silence",
});

export const othersDB: MarkdownDB = markdowndb({
  markdownDir: "others",
  mode: "static",
  publicURL: "/home",
  logLevel: "silence",
});

export const papersDB: MarkdownDB = markdowndb({
  markdownDir: "papers",
  mode: "static",
  publicURL: "/home",
  logLevel: "silence",
});

export const allDB: MarkdownDB = new AllDB([
  articlesDB,
  notesDB,
  othersDB,
  papersDB
]);

export function chronoList(md: MarkdownDB): Array<Markdown> {
  const times = Array.from(md.keys("time")!).sort().reverse();
  const flatten = flat(
    times
      .map(k => md.get(new Date(k)))
      .filter(ml => ml !== undefined) as Array<Array<Markdown>>);
  return unique(flatten, (m) => m.header.id);
}

export const chronoLists = {
  otherChronoList: chronoList(othersDB),
  noteChronoList: chronoList(notesDB),
  articleChronoList: chronoList(articlesDB),
};

export type {
  Markdown,
  MarkdownDB,
  MarkdownText,
  MarkdownHeader,
  MarkdownRuntimeDatabase
} from 'markdowndb.macro';
