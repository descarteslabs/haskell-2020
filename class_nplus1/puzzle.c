#include <stdio.h>

void solve()
{
  for (int s=1; s<10; s++) {
    for (int e=0; e<10; e++) {
      if (e == s) continue;
      for (int n=0; n<10; n++) {
        if (n == s || n == e) continue;
        for (int d=0; d<10; d++) {
          if (d == s || d == e || d == n) continue;
          int send = 1000*s + 100*e + 10*n + d;

          for (int m=1; m<10; m++) {
            if (m == s || m == e || m == n || m == d) continue;
            for (int o=0; o<10; o++) {
              if (o == s || o == e || o == n || o == d || o == m) continue;
              for (int r=0; r<10; r++) {
                if (r == s || r == e || r == n || r == d || r == m || r == o) continue;
                int more = 1000*m + 100*o + 10*r + e;

                for (int y=0; y<10; y++) {
                  if (y == s || y == e || y == n || y == d || y == m || y == o || y == r) continue;
                  int money = 10000*m + 1000*o + 100*n + 10*e + y;

                  if (send + more == money) {
                    printf("   %d%d%d%d\n", s, e, n, d);
                    printf("+  %d%d%d%d\n", m, o, r, e);
                    printf("-------\n");
                    printf("  %d%d%d%d%d\n", m, o, n, e, y);
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

void main()
{
  solve();
}
