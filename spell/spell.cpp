#include <iostream>
#include <fstream>
#include <iterator>
#include <algorithm>
#include <unordered_set>
#include <cctype>
using namespace std;

typedef istream_iterator<string> string_iterator;
typedef unordered_set<string> used_set;
used_set dict;

string clean(const string &s) {
    string res;
    for (char c : s) 
        if (isalpha(c) || (c == '\''))
            res += (char)tolower(c);
    return res;
}

void check(const string & s) {
    string cleanword = clean(s);
    if (! dict.count(cleanword) )
        cout << s << endl;
}

void fill_dict(vector<string> &dv) {
    ifstream dictin("/usr/share/dict/words");
    while (dictin) {
        string s;
        dictin >> s;
        dv.push_back(s);
    }
    dictin.close();
}

inline void check_stream(istream &checkin) {
    for_each(string_iterator(checkin), string_iterator(), check);
}

int main(int argc, char **argv) {
    ifstream fin(argv[1]);
    if (!fin) {
        cerr << "No file " << argv[1] << endl;
        return -1;
    }

    vector<string> dv;
    fill_dict(dv);
    dict = used_set(begin(dv), end(dv));

    check_stream(fin);
}
