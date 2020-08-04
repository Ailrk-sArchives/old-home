import {
  MarkdownRuntimeDatabase,
  MarkdownStaticDatabase,
} from 'markdowndb.macro/dist/markdown-map';
import markdowndb, {Markdown, MarkdownDB} from 'markdowndb.macro';
import {unique, flat} from '../untils/list-ops';
import {AllDB} from './alldb';

export const articlesDB: MarkdownDB = markdowndb('articles', 'runtime');
export const notesDB: MarkdownDB = markdowndb("notes", "static", "/home");

export const allDB: MarkdownDB = new AllDB([
  articlesDB,
  notesDB,
]);

export function chronoList(md: MarkdownDB): Array<Markdown> {
  const times = Array.from(md.keys("time")!).sort().reverse();
  const flatten = flat(
    times
      .map(k => md.get(new Date(k)))
      .filter(ml => ml !== undefined) as Array<Array<Markdown>>);
  return unique(flatten, (m) => m.header.id);
}

export type {
  Markdown,
  MarkdownDB,
  MarkdownText,
  MarkdownHeader,
  MarkdownRuntimeDatabase
} from 'markdowndb.macro';
