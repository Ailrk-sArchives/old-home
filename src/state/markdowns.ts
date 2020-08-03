import markdowndb, {MarkdownDB, Markdown} from 'markdowndb.macro';
import {MarkdownRuntimeDatabase, MarkdownStaticDatabase} from 'markdowndb.macro/dist/markdown-map';
import {flat} from '../untils/flat';

export const articlesDB: MarkdownDB = markdowndb('articles', 'runtime');
export const staticDB: MarkdownDB = markdowndb("notes", "static");
console.log(Array.from(staticDB.values("default")!));
export function chronoList(): Array<Markdown> {
  const times = Array.from(articlesDB.keys("time")!).sort();
  return flat(
    times
      .map(k => articlesDB.get(new Date(k)))
      .filter(ml => ml !== undefined) as Array<Array<Markdown>>).reverse()
}

export type {
  Markdown,
  MarkdownDB,
  MarkdownText,
  MarkdownHeader,
  MarkdownRuntimeDatabase
} from 'markdowndb.macro';
