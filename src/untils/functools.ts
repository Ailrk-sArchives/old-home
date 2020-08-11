export function debounce(fn: (...args: any[]) => void, wait: number) {
  let timeout: NodeJS.Timeout | null;
  return (...args: any[]) => {
    const later = () => {
      timeout = null;
      fn(args);
    }
    timeout = (() => {
      if (timeout) {
        clearTimeout(timeout);
      }
      return setTimeout(later, wait)
    })();
    if (!timeout) fn(args);
  }
}
