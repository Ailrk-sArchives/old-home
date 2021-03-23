"""
Because of how create-react-app is structured, to watch
a md file you need to put it under src directory.
This script automatically move the current md file to
src/assets/abuf and replace the original file with a symlink
to it, so create-react-app can keep track of it's change.
Usage:
    python track <filename>.md
    python track -n <filename>.md
"""
import pathlib
from pathlib import Path
import os
import argparse


root: Path = Path(__file__).parent.resolve().parent
abuf: Path = root / "src" / "assets" / "abuf"
articles: Path = root / "articles"


class PathPipe:
    """
    persistent_path is the real path, e.g articles.
    track_path is the abuf path
    """

    def __init__(self, pathname: str):
        self.pathname: str = pathname

        self.persistent_path: Path = Path(self.pathname)
        self.filename: str = self.persistent_path.name
        self.track_path: Path = abuf / self.filename


def track(pp: PathPipe):
    if pp.persistent_path.is_symlink():
        raise RuntimeError("cannot track a symlink")
    if not pp.track_path.exists():
        # note replace is move
        pp.persistent_path.replace(pp.track_path)

    pp.persistent_path.symlink_to(pp.track_path)


def untrack(pp: PathPipe):
    print(pp.persistent_path.absolute())
    if (not pp.persistent_path.exists()):
        raise FileNotFoundError
    if (not pp.persistent_path.is_symlink()):
        raise RuntimeError("need to untrack a symlink")
    if (not pp.track_path.exists()):
        raise RuntimeError("No file in abuf to untrack")

    if (pp.persistent_path.is_symlink()):
        pp.persistent_path.unlink()
        pp.track_path.replace(pp.persistent_path)


def parse_args() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        usage="%(prog)s [OPTION]",
        description="watch given markdown file with creacte react app")
    parser.add_argument(
        "-v", "--version", action="version",
        version=f"{parser.prog} verseion 0.0.1")
    parser.add_argument("-file", help="file name")

    parser.add_argument(
        '-untrack',
        nargs="?",
        default=argparse.SUPPRESS,
        help="untrack the file")
    return parser


if __name__ == "__main__":
    parser = parse_args()
    args = parser.parse_args()
    filename: str = getattr(args, 'file')

    pp = PathPipe(filename)

    if hasattr(args, 'untrack'):
        untrack(pp)
    else:
        track(pp)
