import type {MarkdownDB, Markdown, ViewType} from 'markdowndb.macro/dist/types';
import {notUndefined} from '../untils/typeguards';
import {flat, chainIterators} from '../untils/list-ops';

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

  entries(view: "default"): IterableIterator<[number, Markdown]> | undefined;
  entries(view: "time" | "tag"): IterableIterator<[string, Array<Markdown>]> | undefined;
  entries(view: ViewType):
    | (IterableIterator<[number, Markdown]> | undefined)
    | (IterableIterator<[string, Array<Markdown>]> | undefined) {
    if (view === "default") return this.megaIterators(m => m.entries(view));
    else return this.megaIterators(m => m.entries(view));
  }

  values(view: "default"): IterableIterator<Markdown> | undefined;
  values(view: "time" | "tag"): IterableIterator<Array<Markdown>> | undefined;
  values(view: ViewType):
    | IterableIterator<Markdown> | undefined
    | IterableIterator<Array<Markdown>> | undefined {
    if (view === "default") return this.megaIterators(m => m.values(view));
    else return this.megaIterators(m => m.values(view));
  }

  keys(view: "default"): IterableIterator<number> | undefined;
  keys(view: "time" | "tag"): IterableIterator<string> | undefined;
  keys(view: ViewType):
    | IterableIterator<number> | undefined
    | IterableIterator<string> | undefined {
    if (view === "default") return this.megaIterators(m => m.keys(view));
    else return this.megaIterators(m => m.keys(view));
  }

  megaIterators<T>(cb: (m: MarkdownDB) => IterableIterator<T> | undefined) {
    const makeIteratorArray =
      <T>(cb: (m: MarkdownDB) => IterableIterator<T> | undefined) =>
        this.list!.map(cb)!.filter(notUndefined);
    return chainIterators(makeIteratorArray(cb));
  }
}
