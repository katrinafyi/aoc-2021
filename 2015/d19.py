#!/usr/bin/env python3.9


from collections import defaultdict
from typing import TypeVar
import re

Data = tuple[dict[str,list[str]], str]

def parse(data: str) -> Data:
    d: dict[str,list[str]] = defaultdict(list)
    mol = ''
    for l in data.strip().split('\n'):
        if not l: continue
        if '=>' in l:
            k, v = l.split(' => ')
            d[k].append(v)
        else:
            mol = l
    return d, mol

def solve1(data: Data):
    repls, mol = data
    x: set[str] = set()
    for old, news in repls.items():
        for new in news:
            i = mol.find(old, 0)
            while i >= 0:
                x.add(mol[:i] + new + mol[i+len(old):])
                i = mol.find(old, i+1)
    print(len(x))

T = TypeVar('T')

def sliding(l: str, k: int) -> list[str]:
    return [l[i:i+k] for i in range(len(l)-k+1)]


def find_terminal(data: Data, n: int) -> set[str]:
    repls, mol = data
    nonterm: set[str] = set()
    produced: set[str] = set()
    for old, _ in repls.items():
        nonterm.update(sliding(old, n))
    produced.update(sliding(mol, n))

    return produced - nonterm

def solve2(data: Data):
    repls, mol = data
    term1 = find_terminal(data, 1)
    print(term1)
    pat1 = re.compile('('+'|'.join(term1)+')')
    print(pat1.sub(r'(\1)', mol))

    nonterm_repls: dict[str,set[str]] = defaultdict(set)



if __name__ == '__main__':
    input = parse(open('19.txt').read())

    print(input)
    solve1(input)
    solve2(input)
