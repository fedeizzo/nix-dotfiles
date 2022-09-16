from os import getenv, listdir
from pathlib import PosixPath


if __name__ == "__main__":
    kindle_dir = PosixPath(f'{getenv("HOME")}/docs/library/Kindle')
    clipping_dir = kindle_dir / list(
        filter(lambda dir: dir.startswith("My Clippings"), listdir(kindle_dir))
    )[0]
    clipping_file = clipping_dir / 'My Clippings - Kindle.txt'

    with open(clipping_file, 'r') as f:
        book =  ""
        note_desc = ""
        for l in f:
            if l.replace('\n', '') != '':
                print(fr'{l}')
            # if 
