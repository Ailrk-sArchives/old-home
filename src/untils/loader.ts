export function lazyload(modName: string, exportName: string) {
  return async () => {
    const mod = await import(modName);

  console.log(mod);
    return {
      default: mod[exportName]
    }
  }
}
