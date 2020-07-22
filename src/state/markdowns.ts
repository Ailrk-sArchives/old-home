import markdowndb, {MarkdownDB, Markdown} from 'markdowndb.macro';
import {flat} from '../untils/flat';

export const markdownDB: MarkdownDB = markdowndb('articles');
export const {db, indexTag, indexTime} = markdownDB;

export function chronoList(): Array<Markdown> {
  const times = Array.from(indexTime.keys()).sort();
  return flat(
    times
      .map(k => indexTime.get(k))
      .filter(ml => ml !== undefined) as Array<Array<Markdown>>).reverse()
}

export type {Markdown, MarkdownDB, MarkdownText, MarkdownHeader} from 'markdowndb.macro';
