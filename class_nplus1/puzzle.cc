#include <iostream>
#include <vector>
#include <algorithm>
#include <iterator>

using namespace std;

using list_t = vector<int>;

list_t remove(const list_t &as, const list_t &bs)
{
  list_t filtered;
  copy_if(bs.begin(), bs.end(), back_inserter(filtered),
          [&as](const int &b){return find(as.begin(), as.end(), b) == as.end();});
  return filtered;
}

int list_to_num(const list_t &as)
{
  int acc = 0;
  for (const auto &a: as) {
    acc *= 10;
    acc += a;
  }
  return acc;
}

list_t bind(const list_t &xs, auto f)
{
  auto acc = list_t();
  for (const auto &x: xs) {
    auto y = f(x);
    acc.insert(acc.end(), y.begin(), y.end());
  }
  return acc;
}

int main() {
  auto digits = list_t({0, 1, 2, 3, 4, 5, 6, 7, 8, 9});
  auto solution = bind
    (remove(list_t({1}), digits),
     [&](const int &s) {
      return bind
        (remove(list_t({s}), digits),
         [&](const int &e) {
          return bind
            (remove(list_t({s, e}), digits),
             [&](const int &n) {
              return bind
                (remove(list_t({s, e, n}), digits),
                 [&](const int &d) {
                  int send = list_to_num(list_t({s, e, n, d}));
                  return bind
                    (remove(list_t({0, s, e, n, d}), digits),
                     [&](const int &m) {
                      return bind
                        (remove(list_t({s, e, n, d, m}), digits),
                         [&](const int &o) {
                          return bind
                            (remove(list_t({s, e, n, d, m, o}), digits),
                             [&](const int &r) {
                              int more = list_to_num(list_t({m, o, r, e}));
                              return bind
                                (remove(list_t({s, e, n, d, m, o, r}), digits),
                                 [&](const int &y) {
                                  int money = list_to_num(list_t({m, o, n, e, y}));
                                  if (send + more == money)
                                    return list_t({send, more, money});
                                  return list_t();
                                });
                            });
                        });
                    });
                });
            });
        });
    });

  cout << "  " << solution[0] << endl;
  cout << "+ " << solution[1] << endl;
  cout << "------" << endl;
  cout << " " << solution[2] << endl;
}
