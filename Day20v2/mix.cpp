#include "mix.h"

#include <list>
#include <iterator>

using std::list;
using std::advance;

int64_t *mix(int64_t *coords, int length, int rounds) {
    list<int64_t> l = list<int64_t>();
    list<int64_t>::iterator *a = new list<int64_t>::iterator[length];
    for (int i = 0; i < length; i++) {
        a[i] = l.insert(l.end(), coords[i]);
    }

    for (int r = 0; r < rounds; r++) {
        for (int i = 0; i < length; i++) {
            int64_t value = *a[i];
            int dist = (int)((value % (length - 1) + (length - 1)) % (length - 1));

            list<int64_t>::iterator it = a[i];
            for (int d = 0; d <= dist; d++) {
                advance(it, 1);
                if (it == l.end()) it = l.begin();
            }

            l.erase(a[i]);
            a[i] = l.insert(it, value);
        }
    }

    int64_t *result = new int64_t[length];
    list<int64_t>::iterator it = l.begin();
    for (int i = 0; i < length; i++) {
        result[i] = *it;
        advance(it, 1);
    }

    delete[] a;

    return result;
}