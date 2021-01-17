from pathlib import Path
from typing import List

root_path: Path = Path('../' + __file__).resolve()
src_abuf_path: Path = root_path / 'src' / 'assets' / 'article_buf'
dest_abuf_path: Path = root_path / 'article_buf'


def read_buf() -> List[Path]:
    buf = []
    for a in src_abuf_path.glob("*.md"):
        buf.append(a)
    return buf


def create_links(bufarticles: List[Path]):
    for n in bufarticles:
        p = dest_abuf_path / n.name
        n.symlink_to(p)


if __name__ == "__main__":
    abuf = read_buf()
    create_links(abuf)
