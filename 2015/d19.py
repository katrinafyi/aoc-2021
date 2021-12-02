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

def solve1(data: Data) -> set[str]:
    repls, mol = data
    x: set[str] = set()
    for old, news in repls.items():
        for new in news:
            i = mol.find(old, 0)
            while i >= 0:
                x.add(mol[:i] + new + mol[i+len(old):])
                i = mol.find(old, i+1)
    # print(len(x))
    return x

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

def subseq(a: list[str], b: list[str], ai: int = 0, bi: int = 0) -> bool:
    ai = bi = 0
    while ai < len(a) and bi < len(b):
        if a[ai] == b[bi]:
            ai += 1
            bi += 1
        else:
            bi += 1
    return ai == len(a)


def solve2(data: Data):
    repls, mol = data
    term1 = find_terminal(data, 1)
    print(term1)
    pat1 = re.compile('('+'|'.join(term1)+')')
    print(pat1.sub(r'(\1)', mol))

    def trace(s: str):
        return pat1.findall(s)

    INF = 1 << 31
    trace_mol = trace(mol)
    def go(m: str) -> int:
        if len(m) > len(mol): return INF
        t = trace(m)
        if not subseq(t, trace_mol): return INF
        if len(m) == len(mol) and m == mol: return 0

        min_steps = INF
        for m2 in solve1((repls, m)):
            g2 = go(m2)
            if g2 < min_steps:
                min_steps = g2
        return min_steps + 1

    print(trace(mol))
    print(go(mol))
    print(go('CRnCaCaCaSiRnBPTiMgArSiRnSiRnMgArSiRnCaFArTiTiBSiThFYCaFArCaCaSiThCaPBSiThSiThCaCaPTiRnPBSiThRnFArArCaCaSiThCaSiThSiRnMgArCaPTiBPRnFArSiThCaSiRnFArBCaSiRnCaPRnFArPMgYCaFArCaPTiTiTiBPBSiThCaPTiBPBSiRnFArBPBSiRnCaFArBPRnSiRnFArRnSiRnMgArCaFArCaCaCaSiThSiThCaCaPBPTiTiRnFArCaPTiBSiAlArPBCaCaCaCaCaSiRnMgArCaSiThFArThCaSiThCaSiRnCaFYCaSiRnFYFArFArCaSiRnFYFArCaSiRnBPMgArSiThPRnFArCaSiRnFArTiRnSiRnFYFArCaSiRnMgArCaSiRnTiMgArSiThCaSiThCaFArPRnFArSiRnFArTiTiTiTiBCaCaSiRnCaCaFYFArSiThCaPTiBPTiBCaSiThSiRnMgArCaF'))

if __name__ == '__main__':
    input = parse(open('19.txt').read())

    print(input)
    solve1(input)
    solve2(input)
