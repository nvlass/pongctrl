#!/usr/bin/env python
import sys
import random


if __name__ == '__main__':
    try:
        y0 = 0
        while True:
            fmax = 0
            b = sys.stdin.readline()
            width = int(b.split(" ")[0])
            height = int(b.split(" ")[1])
            for q in range(height):
                a = sys.stdin.readline().strip()
                
                bq = a[8:50] + a[-50:-8]
                t = sum(map(lambda x: int(x), list(bq)))
                if t > fmax:
                    y0 = q
                    fmax = t
            sys.stdout.write(str(y0) + "\n")
            sys.stdout.flush()
    except Exception as ex:
        sys.stderr.write(str(ex))
        sys.stderr.flush()


