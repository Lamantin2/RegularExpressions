#include <iostream>
#include <vector>
#include <unordered_map>
#include <deque>
#include <unordered_set>
#include <queue>
#include <set>

struct NFA {
  struct Trans {
    char c;
    int to;
  };

  std::vector<std::vector<Trans>> adj;
  int start = -1;
  int accept = -1;

  int new_state() { 
    adj.emplace_back();
    return (int)adj.size() - 1;
  }

  void add_transition(int u, int v, char c) {
    adj[u].push_back({c, v});
  }
};

static bool is_literal(char ch) {
  if (ch == '+' || ch == '*' || ch == '(' || ch == ')' || ch == '.')
    return false;
  return true;
}

static std::string insert_concat(const std::string &re) {
  std::string out;

  for (size_t i = 0; i < re.size(); ++i) {
    char c = re[i];
    out.push_back(c);
    if (i + 1 < re.size()) {
      char d = re[i+1];
      bool left = is_literal(c) || c == '*' || c == ')';
      bool right = is_literal(d) || d == '(';
      if (left && right)
        out.push_back('.');
    }
  }

  return out;
}

static int prec(char op) {
  if (op == '*')
    return 3;
  if (op == '.')
    return 2;
  if (op == '+')
    return 1;
  return 0;
}

static std::string to_postfix(const std::string &re) {
  std::string s = insert_concat(re);
  std::string out;
  std::vector<char> st;

  for (size_t i = 0; i < s.size(); ++i) {
    char c = s[i];
    if (is_literal(c)) {
      out.push_back(c);
    } else if (c == '(') {
      st.push_back(c);
    } else if (c == ')') {
      while (!st.empty() && st.back() != '(') {
        out.push_back(st.back());
        st.pop_back();
      }
      if (!st.empty() && st.back() == '(')
        st.pop_back();
      else throw std::runtime_error("Mismatched parentheses");
    } else {
      if (c == '*') {
        while (!st.empty() && prec(st.back()) > prec(c)) {
          out.push_back(st.back());
          st.pop_back();
        }
        st.push_back(c);
      } else {
        while (!st.empty() && prec(st.back()) >= prec(c)) {
          out.push_back(st.back());
          st.pop_back();
        }
        st.push_back(c);
      }
    }
  }
  while (!st.empty()) {
    if (st.back() == '(' || st.back() == ')')
      throw std::runtime_error("скобки");
    out.push_back(st.back());
    st.pop_back();
  }
  return out;
}

struct Frag {
  int s;
  int e;
  Frag(int s_= -1, int e_ = -1): s(s_), e(e_) {}
};

static NFA build_nfa_from_postfix(const std::string &postfix) {
  NFA nfa;
  std::vector<Frag> stk;

  for (char c : postfix) {
    if (is_literal(c)) {
      int a = nfa.new_state();
      int b = nfa.new_state();
      nfa.add_transition(a, b, c);
      stk.emplace_back(a,b);
    } else if (c == '.') {
      if (stk.size() < 2)
        throw std::runtime_error("стек переполнен");
      Frag b = stk.back(); stk.pop_back();
      Frag a = stk.back(); stk.pop_back();
      nfa.add_transition(a.e, b.s, 0);
      stk.emplace_back(a.s, b.e);
    } else if (c == '+') {
      if (stk.size() < 2)
        throw std::runtime_error("стек переполнен");
      Frag b = stk.back(); stk.pop_back();
      Frag a = stk.back(); stk.pop_back();
      int s = nfa.new_state();
      int e = nfa.new_state();
      nfa.add_transition(s, a.s, 0);
      nfa.add_transition(s, b.s, 0);
      nfa.add_transition(a.e, e, 0);
      nfa.add_transition(b.e, e, 0);
      stk.emplace_back(s, e);
    } else if (c == '*') {
      if (stk.empty())
        throw std::runtime_error("стек переполнен");
      Frag a = stk.back(); stk.pop_back();
      int s = nfa.new_state();
      int e = nfa.new_state();
      nfa.add_transition(s, a.s, 0);
      nfa.add_transition(s, e, 0);
      nfa.add_transition(a.e, a.s, 0);
      nfa.add_transition(a.e, e, 0);
      stk.emplace_back(s, e);
    } else {
      throw std::runtime_error(std::string("Неизвестная лажа: ") + c);
    }
  }
  if (stk.size() != 1)
    throw std::runtime_error("размер стека != 1");
  Frag res = stk.back();
  nfa.start = res.s;
  nfa.accept = res.e;
  return nfa;
}

static void epsilon_closure_from_initial(const NFA &nfa, const std::vector<int> &initial, std::vector<char> &out) {
  int n = (int)nfa.adj.size();
  out.assign(n, 0);
  std::deque<int> dq;
  for (int v : initial) {
    if (v >= 0 && v < n && !out[v]) { 
      out[v] = 1; dq.push_back(v);
    }
  }
  while (!dq.empty()) {
    int v = dq.front(); dq.pop_front();
    for (auto &tr : nfa.adj[v]) {
      if (tr.c == 0 && !out[tr.to]) {
        out[tr.to] = 1;
        dq.push_back(tr.to);
      }
    }
  }
}

static void epsilon_closure_from_set(const NFA &nfa, const std::vector<char> &inset, std::vector<char> &out) {
  int n = (int)nfa.adj.size();
  out.assign(n, 0);
  std::deque<int> dq;
  for (int i = 0; i < n; ++i) {
    if (inset[i]) {
      out[i] = 1; 
      dq.push_back(i);
    }
  }
  while (!dq.empty()) {
    int v = dq.front(); dq.pop_front();
    for (auto &tr : nfa.adj[v]) {
      if (tr.c == 0 && !out[tr.to]) {
        out[tr.to] = 1;
        dq.push_back(tr.to);
      }
    }
  }
}

static std::vector<char> move_on_symbol(const NFA &nfa, const std::vector<char> &inset, char c) {
  int n = (int)nfa.adj.size();
  std::vector<char> tmp(n, 0);
  for (int i = 0; i < n; ++i) {
    if (inset[i]) {
      for (auto &tr : nfa.adj[i]) {
        if (tr.c == c) tmp[tr.to] = 1;
      }
    }
  }
  std::vector<char> out;
  epsilon_closure_from_set(nfa, tmp, out);
  return out;
}

static bool any_marked(const std::vector<char> &v) {
  for (char c : v)
    if (c)
      return true;
  return false;
}

static bool has_accept(const std::vector<char> &setv, int accept) {
  if (accept < 0 || accept >= (int)setv.size())
    return false;
  return setv[accept];
}

static int shortest_with_xk(const NFA &nfa, const std::vector<char> &alphabet, char x, int k) {
  if (k <= 0)
    return 0;
  int nstates = (int)nfa.adj.size();
  if (nstates == 0)
    return -1;

  std::vector<int> starts = { nfa.start };
  std::vector<char> start_set;
  epsilon_closure_from_initial(nfa, starts, start_set);

  auto pack_key = [&](const std::vector<char> &setv, int t) -> std::string {
    std::string s;
    s.resize(nstates + 1);
    for (int i = 0; i < nstates; ++i)
      s[i] = setv[i];
    s[nstates] = (char) t;
    return s;
  };

  std::queue<std::pair<std::vector<char>, int>> q;
  std::queue<int> distq;
  std::unordered_set<std::string> visited;

  int t0 = 0;
  std::string k0 = pack_key(start_set, t0);
  visited.insert(k0);
  q.emplace(start_set, t0);
  distq.push(0);

  if (t0 == k && has_accept(start_set, nfa.accept))
    return 0;

  while (!q.empty()) {
    auto cur = q.front(); q.pop();
    int dist = distq.front(); distq.pop();
    const std::vector<char> cur_set = cur.first;
    int t = cur.second;

    for (char a : alphabet) {
      std::vector<char> next_set = move_on_symbol(nfa, cur_set, a);
      if (!any_marked(next_set))
        continue;

      int t2 = (a == x) ? std::min(k, t + 1) : 0;

      if (t2 == k && has_accept(next_set, nfa.accept)) {
        return dist + 1;
      }

      std::string key = pack_key(next_set, t2);
      if (!visited.count(key)) {
        visited.insert(key);
        q.emplace(next_set, t2);
        distq.push(dist + 1);
      }
    }
  }

  return -1;
}

static std::vector<char> collect_alphabet_from_nfa(const NFA &nfa) {
  std::set<char> s;
  for (auto &vec : nfa.adj)
    for (auto &tr : vec)
      if (tr.c != 0) s.insert(tr.c);
  std::vector<char> out(s.begin(), s.end());
  return out;
}

int solve_from_input(const std::string &alpha, char x, int k) {
  std::string postfix;
  try {
    postfix = to_postfix(alpha);
  } catch (...) {
    return -1;
  }
  NFA nfa;
  try {
    nfa = build_nfa_from_postfix(postfix);
  } catch (...) {
    return -1;
  }
  std::vector<char> alphabet = collect_alphabet_from_nfa(nfa);
  bool hasx = false;
  for (char c : alphabet)
    if (c == x) {
      hasx = true;
      break;
    }
  if (!hasx)
    return -1;
  return shortest_with_xk(nfa, alphabet, x, k);
}

static void expect_eq(int got, int want, const std::string &msg, int &fails) {
  if (got != want) {
    std::cerr << "Потрачено: " << msg << " | есть=" << got << " надо=" << want << "\n";
    ++fails;
  }
}

static int run_unit_tests() {
  int fails = 0;
  expect_eq(solve_from_input("a", 'a', 1), 1, "'a' содержит 'a' 1 раз", fails);
  expect_eq(solve_from_input("a", 'a', 2), -1, "'a' не может содержать 'aa'", fails);
  expect_eq(solve_from_input("aa", 'a', 2), 2, "'aa' содержит 'aa' длины 2", fails);
  expect_eq(solve_from_input("ab", 'b', 1), 2, "'ab' содержит 'b' длины 2", fails);
  expect_eq(solve_from_input("a|b", 'b', 1), 1, "'a|b' содержит 'b' длины 1", fails);
  expect_eq(solve_from_input("ab|ba", 'a', 1), 2, "'ab|ba' min с 'a' длина 2", fails);
  expect_eq(solve_from_input("a*", 'a', 3), 3, "'a*' может производить 'aaa'", fails);
  expect_eq(solve_from_input("(ab)*", 'a', 2), -1, "'(ab)*' не может иметь 'aa' смежно йо", fails);
  expect_eq(solve_from_input("(a|b)*", 'a', 5), 5, "(a|b)* может производить aaaaa", fails);
  expect_eq(solve_from_input("a*b*", 'b', 2), 2, "'a*b*' может производить 'bb'", fails);
  expect_eq(solve_from_input("(a|b)c*", 'c', 2), 3, "'(a|b)c*' min содерж 'cc' длины 3", fails);
  expect_eq(solve_from_input("((a)*)", 'a', 1), 1, "вложенная звёздочка", fails);
  expect_eq(solve_from_input("b*", 'a', 1), -1, "'b*' не может производить 'a'", fails);
  expect_eq(solve_from_input("(ab|a)*", 'a', 2), 2, "язык может иметь 'aa' через выбор", fails);
  expect_eq(solve_from_input("a|a*", 'a', 4), 4, "a* позволяет 4 a", fails);
  expect_eq(solve_from_input("(ab)*|(cd)", 'b', 1), 2, "из (ab)* min 'ab' длины 2", fails);

  if (fails == 0) {
    std::cout << "ПРОЙДЕНО!!!!!!!!\n";
  } else {
    std::cerr << fails << " тестов не пройдено(((((((\n";
  }
  return fails;
}

int main(int argc, char **argv) {
  std::ios::sync_with_stdio(false);
  std::cin.tie(nullptr);

  if (argc >= 2) {
    std::string arg = argv[1];
    if (arg == "--run-tests") {
      int r = run_unit_tests();
      return (r == 0 ? 0 : 1);
    }
  }

  std::string alpha;
  if (!getline(std::cin, alpha))
    return 0;
  std::string xs;
  if (!getline(std::cin, xs))
    return 0;
  std::string ks;
  if (!getline(std::cin, ks))
    return 0;

  auto trim = [&](std::string s)->std::string {
    size_t i=0; while (i<s.size() && isspace((unsigned char)s[i])) ++i;
    size_t j=s.size(); while (j>i && isspace((unsigned char)s[j-1])) --j;
    return s.substr(i, j-i);
  };
  alpha = trim(alpha);
  xs = trim(xs);
  ks = trim(ks);
  if (xs.empty()) {
    std::cout << -1 << "\n";
    return 0;
  }
  char x = xs[0];
  int k;
  try {
    k = stoi(ks);
  } catch (...) {
    std::cout << -1 << "\n"; return 0;
  }
  int ans = solve_from_input(alpha, x, k);
  std::cout << ans << "\n";
}
