import markdowndb, {MarkdownDB} from 'markdowndb.macro';

export const markdownDB: MarkdownDB = markdowndb('articles');
export const {db, indexTag} = markdownDB;

export type {Markdown, MarkdownDB, MarkdownText, MarkdownHeader} from 'markdowndb.macro';
