import {
  MarkdownRuntimeDatabase,
  MarkdownStaticDatabase,
  AllDB,
} from 'markdowndb.macro/dist/markdown-map';

import markdowndb, {Markdown, MarkdownDB} from 'markdowndb.macro';
import {unique, flat} from '../untils/list-ops';
import * as watch from './watch';
watch.abuf_dummy();

export const articlesDB: MarkdownDB = markdowndb({
  markdownDir: 'articles',
  mode: 'static',
  publicURL: "/home",
  logLevel: "silence",
});

export const wikiDB: MarkdownDB = markdowndb({
  markdownDir: 'wiki',
  mode: 'static',
  publicURL: "/home",
  logLevel: "silence",
});

// export const notesDB: MarkdownDB = markdowndb({
//   markdownDir: "notes",
//   mode: "static",
//   publicURL: "/home",
//   logLevel: "silence",
// });

// export const othersDB: MarkdownDB = markdowndb({
//   markdownDir: "others",
//   mode: "static",
//   publicURL: "/home",
//   logLevel: "silence",
// });

// export const papersDB: MarkdownDB = markdowndb({
//   markdownDir: "papers",
//   mode: "static",
//   publicURL: "/home",
//   logLevel: "silence",
// });

export const allDB: MarkdownDB = new AllDB([
  articlesDB,
  wikiDB,
  // notesDB,
  // othersDB,
  // papersDB
]);

export function chronoList(md: MarkdownDB): Array<Markdown> {
  const times = Array.from(md.keys("time")!).sort().reverse();
  const flatten = flat(
    times
      .map(k => md.get(new Date(k))
      )
      .filter(ml => ml !== undefined) as Array<Array<Markdown>>)
  return unique(flatten, (m) => m.header.id);
}

export const chronoLists = {
  // otherChronoList: chronoList(othersDB),
  // noteChronoList: chronoList(notesDB),
  wikiChronoLists: chronoList(wikiDB),
  articleChronoList: chronoList(articlesDB),
};

export type {
  Markdown,
  MarkdownDB,
  MarkdownText,
  MarkdownHeader,
  MarkdownRuntimeDatabase
} from 'markdowndb.macro';
