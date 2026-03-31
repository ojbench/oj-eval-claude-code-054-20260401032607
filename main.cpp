#include <iostream>
#include <vector>
#include <algorithm>
#include <cstring>
#include <string>
using namespace std;

// Term: represents a * x^b * sin^c(x) * cos^d(x)
struct Term {
    int a, b, c, d;
    Term(int _a = 0, int _b = 0, int _c = 0, int _d = 0) : a(_a), b(_b), c(_c), d(_d) {}

    // For sorting: compare by (b, c, d) in descending order
    bool operator<(const Term& other) const {
        if (b != other.b) return b > other.b;
        if (c != other.c) return c > other.c;
        return d > other.d;
    }

    // For merging like terms: same if (b, c, d) are equal
    bool same_type(const Term& other) const {
        return b == other.b && c == other.c && d == other.d;
    }
};

// Polynomial: sum of terms
struct Poly {
    vector<Term> terms;

    Poly() {}
    Poly(const Term& t) { terms.push_back(t); }

    void simplify() {
        if (terms.empty()) return;

        // Sort terms
        sort(terms.begin(), terms.end());

        // Merge like terms
        vector<Term> result;
        for (const Term& t : terms) {
            if (!result.empty() && result.back().same_type(t)) {
                result.back().a += t.a;
            } else {
                result.push_back(t);
            }
        }

        // Remove zero terms
        terms.clear();
        for (const Term& t : result) {
            if (t.a != 0) terms.push_back(t);
        }
    }

    Poly operator+(const Poly& other) const {
        Poly result;
        result.terms = terms;
        result.terms.insert(result.terms.end(), other.terms.begin(), other.terms.end());
        result.simplify();
        return result;
    }

    Poly operator-(const Poly& other) const {
        Poly result;
        result.terms = terms;
        for (const Term& t : other.terms) {
            result.terms.push_back(Term(-t.a, t.b, t.c, t.d));
        }
        result.simplify();
        return result;
    }

    Poly operator*(const Poly& other) const {
        Poly result;
        for (const Term& t1 : terms) {
            for (const Term& t2 : other.terms) {
                result.terms.push_back(Term(t1.a * t2.a, t1.b + t2.b, t1.c + t2.c, t1.d + t2.d));
            }
        }
        result.simplify();
        return result;
    }

    // Derivative of polynomial
    Poly derivate() const {
        Poly result;
        for (const Term& t : terms) {
            // d/dx(a * x^b * sin^c(x) * cos^d(x))
            // = a * [b*x^(b-1)*sin^c(x)*cos^d(x) + x^b*c*sin^(c-1)(x)*cos(x)*cos^d(x) + x^b*sin^c(x)*(-d)*sin(x)*cos^(d-1)(x)]

            // Term 1: from x^b -> b * x^(b-1)
            if (t.b > 0) {
                result.terms.push_back(Term(t.a * t.b, t.b - 1, t.c, t.d));
            }

            // Term 2: from sin^c(x) -> c * sin^(c-1)(x) * cos(x)
            if (t.c > 0) {
                result.terms.push_back(Term(t.a * t.c, t.b, t.c - 1, t.d + 1));
            }

            // Term 3: from cos^d(x) -> -d * sin(x) * cos^(d-1)(x)
            if (t.d > 0) {
                result.terms.push_back(Term(-t.a * t.d, t.b, t.c + 1, t.d - 1));
            }
        }
        result.simplify();
        return result;
    }

    string to_string() const {
        if (terms.empty()) return "0";

        string result;
        for (size_t i = 0; i < terms.size(); i++) {
            const Term& t = terms[i];

            // Sign and coefficient
            if (i == 0) {
                if (t.a == 1 && (t.b != 0 || t.c != 0 || t.d != 0)) {
                    // Don't print 1
                } else if (t.a == -1 && (t.b != 0 || t.c != 0 || t.d != 0)) {
                    result += "-";
                } else {
                    result += std::to_string(t.a);
                }
            } else {
                if (t.a > 0) {
                    result += "+";
                    if (t.a == 1 && (t.b != 0 || t.c != 0 || t.d != 0)) {
                        // Don't print 1
                    } else {
                        result += std::to_string(t.a);
                    }
                } else {
                    if (t.a == -1 && (t.b != 0 || t.c != 0 || t.d != 0)) {
                        result += "-";
                    } else {
                        result += std::to_string(t.a);
                    }
                }
            }

            // x^b
            if (t.b > 0) {
                result += "x";
                if (t.b > 1) result += "^" + std::to_string(t.b);
            }

            // sin^c(x)
            if (t.c > 0) {
                result += "sin";
                if (t.c > 1) result += "^" + std::to_string(t.c);
                result += "x";
            }

            // cos^d(x)
            if (t.d > 0) {
                result += "cos";
                if (t.d > 1) result += "^" + std::to_string(t.d);
                result += "x";
            }
        }

        return result;
    }
};

// Fraction: p/q
struct Frac {
    Poly p, q;

    Frac() {
        q.terms.push_back(Term(1, 0, 0, 0));
    }

    Frac(int x) {
        p.terms.push_back(Term(x, 0, 0, 0));
        q.terms.push_back(Term(1, 0, 0, 0));
    }

    Frac(const Term& t) {
        p.terms.push_back(t);
        q.terms.push_back(Term(1, 0, 0, 0));
    }

    Frac(const Poly& _p, const Poly& _q) : p(_p), q(_q) {}

    Frac operator+(const Frac& other) const {
        // (p1/q1) + (p2/q2) = (p1*q2 + p2*q1) / (q1*q2)
        return Frac(p * other.q + other.p * q, q * other.q);
    }

    Frac operator-(const Frac& other) const {
        // (p1/q1) - (p2/q2) = (p1*q2 - p2*q1) / (q1*q2)
        return Frac(p * other.q - other.p * q, q * other.q);
    }

    Frac operator*(const Frac& other) const {
        // (p1/q1) * (p2/q2) = (p1*p2) / (q1*q2)
        return Frac(p * other.p, q * other.q);
    }

    Frac operator/(const Frac& other) const {
        // (p1/q1) / (p2/q2) = (p1*q2) / (q1*p2)
        return Frac(p * other.q, q * other.p);
    }

    Frac derivate() const {
        // (p/q)' = (p'*q - q'*p) / (q*q)
        return Frac(p.derivate() * q - q.derivate() * p, q * q);
    }

    void output() const {
        string p_str = p.to_string();
        string q_str = q.to_string();

        // Special case: p = 0
        if (p_str == "0") {
            cout << "0" << endl;
            return;
        }

        // Special case: q = 1
        if (q_str == "1") {
            cout << p_str << endl;
            return;
        }

        // Check if parentheses are needed
        bool p_needs_paren = (p.terms.size() > 1);
        bool q_needs_paren = (q.terms.size() > 1);

        if (p_needs_paren) cout << "(";
        cout << p_str;
        if (p_needs_paren) cout << ")";
        cout << "/";
        if (q_needs_paren) cout << "(";
        cout << q_str;
        if (q_needs_paren) cout << ")";
        cout << endl;
    }
};

// Parse integer from string s[l..r)
int get_number(const string& s, int l, int r) {
    if (l >= r) return 1;

    // Check for sign
    int sign = 1;
    if (s[l] == '-') {
        sign = -1;
        l++;
    } else if (s[l] == '+') {
        l++;
    }

    if (l >= r) return sign;

    int result = 0;
    bool has_digit = false;
    while (l < r && isdigit(s[l])) {
        result = result * 10 + (s[l] - '0');
        has_digit = true;
        l++;
    }

    if (!has_digit) return sign;
    return sign * result;
}

// Parse term from string s[l..r)
Term get_term(const string& s, int l, int r) {
    int a = 0, b = 0, c = 0, d = 0;

    // Find coefficient end
    int coef_end = l;
    if (coef_end < r && (s[coef_end] == '-' || s[coef_end] == '+')) coef_end++;
    while (coef_end < r && isdigit(s[coef_end])) coef_end++;

    a = get_number(s, l, coef_end);

    // Parse x, sin, cos
    int i = coef_end;
    while (i < r) {
        if (i + 3 <= r && s.substr(i, 3) == "sin") {
            i += 3;
            // Check for "x" or "^"
            if (i < r && s[i] == '^') {
                // sin^c form (without x between sin and ^)
                i++;
                int exp_start = i;
                while (i < r && isdigit(s[i])) i++;
                c = get_number(s, exp_start, i);
                // Now expect 'x'
                if (i < r && s[i] == 'x') i++;
            } else if (i < r && s[i] == 'x') {
                i++; // sinx
                if (i < r && s[i] == '^') {
                    // sinx^c form
                    i++;
                    int exp_start = i;
                    while (i < r && isdigit(s[i])) i++;
                    c = get_number(s, exp_start, i);
                } else {
                    c = 1;
                }
            } else {
                c = 1;
            }
        } else if (i + 3 <= r && s.substr(i, 3) == "cos") {
            i += 3;
            // Check for "x" or "^"
            if (i < r && s[i] == '^') {
                // cos^d form (without x between cos and ^)
                i++;
                int exp_start = i;
                while (i < r && isdigit(s[i])) i++;
                d = get_number(s, exp_start, i);
                // Now expect 'x'
                if (i < r && s[i] == 'x') i++;
            } else if (i < r && s[i] == 'x') {
                i++; // cosx
                if (i < r && s[i] == '^') {
                    // cosx^d form
                    i++;
                    int exp_start = i;
                    while (i < r && isdigit(s[i])) i++;
                    d = get_number(s, exp_start, i);
                } else {
                    d = 1;
                }
            } else {
                d = 1;
            }
        } else if (s[i] == 'x') {
            i++;
            if (i < r && s[i] == '^') {
                i++;
                int exp_start = i;
                while (i < r && isdigit(s[i])) i++;
                b = get_number(s, exp_start, i);
            } else {
                b = 1;
            }
        } else {
            i++;
        }
    }

    return Term(a, b, c, d);
}

// Parse expression recursively
Frac dfs(const string& s, int l, int r);

// Find operator at level (not inside parentheses)
int find_op(const string& s, int l, int r, char op) {
    int depth = 0;
    for (int i = r - 1; i >= l; i--) {
        if (s[i] == ')') depth++;
        else if (s[i] == '(') depth--;
        else if (depth == 0 && s[i] == op) {
            // Make sure it's not part of a negative number at the start
            if (op == '-' && i == l) {
                continue;
            }
            // Make sure it's not a negative after an operator
            if (op == '-' && i > l) {
                char prev = s[i-1];
                if (prev == '(' || prev == '+' || prev == '-' || prev == '*' || prev == '/') {
                    continue;
                }
            }
            return i;
        }
    }
    return -1;
}

Frac dfs(const string& s, int l, int r) {
    // Remove outer spaces
    while (l < r && s[l] == ' ') l++;
    while (l < r && s[r-1] == ' ') r--;

    // Remove outer parentheses
    while (l < r && s[l] == '(' && s[r-1] == ')') {
        int depth = 0;
        bool valid = true;
        for (int i = l; i < r; i++) {
            if (s[i] == '(') depth++;
            else if (s[i] == ')') depth--;
            if (depth == 0 && i < r - 1) {
                valid = false;
                break;
            }
        }
        if (valid) {
            l++;
            r--;
        } else {
            break;
        }
    }

    // Try to find +/- at top level (from right to left)
    // Search for the rightmost + or -
    int pos = -1;
    int depth = 0;
    for (int i = r - 1; i >= l; i--) {
        if (s[i] == ')') depth++;
        else if (s[i] == '(') depth--;
        else if (depth == 0 && (s[i] == '+' || s[i] == '-')) {
            // Make sure it's not at the start
            if (i == l) continue;
            // Make sure it's not a negative after an operator
            if (s[i] == '-' && i > l) {
                char prev = s[i-1];
                if (prev == '(' || prev == '+' || prev == '-' || prev == '*' || prev == '/') {
                    continue;
                }
            }
            pos = i;
            break;
        }
    }
    if (pos != -1) {
        if (s[pos] == '+') {
            return dfs(s, l, pos) + dfs(s, pos + 1, r);
        } else {
            return dfs(s, l, pos) - dfs(s, pos + 1, r);
        }
    }

    // Try to find * at top level
    pos = find_op(s, l, r, '*');
    if (pos != -1) {
        return dfs(s, l, pos) * dfs(s, pos + 1, r);
    }

    // Try to find / at top level
    pos = find_op(s, l, r, '/');
    if (pos != -1) {
        return dfs(s, l, pos) / dfs(s, pos + 1, r);
    }

    // Base case: it's a single term
    Term t = get_term(s, l, r);
    return Frac(t);
}

void solve(const string& s) {
    Frac f = dfs(s, 0, s.length());
    f.output();
    f.derivate().output();
}

int main() {
    string s;
    getline(cin, s);
    solve(s);
    return 0;
}
