export function flat<T>(arr: Array<Array<T>>): Array<T> {
  const acc: Array<T> = [];
  for (const sub of arr) {
    for (const ele of sub) {
      acc.push(ele);
    }
  }
  return acc;
}

// O(3n) time.  O(2n) space.
// It's fine, the best way of doing it is sort then tag, which is at least
// O(nlogn).
export function unique<T>(list: Array<T>, key: (val: T) => number): Array<T> {
  const keys = new Set(list.map(key));
  const res: Array<T> = [];
  for (const val of list) {
    const k = key(val);
    if (keys.has(k)) {
      keys.delete(k);
      res.push(val);
    }
  }
  return res;
}

type IteratorList<T> = Array<Iterator<T, T, undefined>>;
export function* chainIterators<T>(iters: IteratorList<T>) {
  for (const iter of iters) {
    let doneFlag: boolean = false;
    while (!doneFlag) {
      const {value, done}= iter.next();
      doneFlag = done ?? true;
      yield value;
    }
  }
}
