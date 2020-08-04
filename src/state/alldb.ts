import type {MarkdownDB, Markdown, ViewType} from 'markdowndb.macro/dist/types';
import {notUndefined} from '../untils/typeguards';
import {flat} from '../untils/list-ops';

export class AllDB implements MarkdownDB {
  list?: Array<MarkdownDB> = undefined;

  public constructor(list: Array<MarkdownDB>) {
    this.list = list;
  }
  get(key: number): Markdown | undefined;
  get(key: Date | string): Array<Markdown> | undefined;
  get(key: number | Date | string):
    | (Markdown | undefined)
    | (Array<Markdown> | undefined) {
    if (typeof key === "number") {
      const filtered = this.list?.map(m => m.get(key)).filter(notUndefined);
      if (filtered!.length > 1) {throw new Error("id not unique");}
      return filtered?.pop();
    }
    return flat(this.list?.map(db => db.get(key)).filter(notUndefined)!);
  }

  entries(view: "default"): Array<[number, Markdown]> | undefined;
  entries(view: "time" | "tag"): Array<[string, Array<Markdown>]> | undefined;
  entries(view: ViewType):
    | (Array<[number, Markdown]> | undefined)
    | (Array<[string, Array<Markdown>]> | undefined) {
    if (view === "default") return this.megaList(m => m.entries(view));
    else return this.megaList(m => m.entries(view));
  }

  values(view: "default"): Array<Markdown> | undefined;
  values(view: "time" | "tag"): Array<Array<Markdown>> | undefined;
  values(view: ViewType):
    | Array<Markdown> | undefined
    | Array<Array<Markdown>> | undefined {
    if (view === "default") return this.megaList(m => m.values(view));
    else return this.megaList(m => m.values(view));
  }

  keys(view: "default"): Array<number> | undefined;
  keys(view: "time" | "tag"): Array<string> | undefined;
  keys(view: ViewType):
    | Array<number> | undefined
    | Array<string> | undefined {
    if (view === "default") return this.megaList(m => m.keys(view));
    else return this.megaList(m => m.keys(view));
  }

  megaList<T>(cb: (m: MarkdownDB) => Array<T> | undefined) {
    const makeIteratorArray =
      <T>(cb: (m: MarkdownDB) => Array<T> | undefined) =>
        this.list!.map(cb)!.filter(notUndefined);
    return flat(makeIteratorArray(cb));
  }
}
