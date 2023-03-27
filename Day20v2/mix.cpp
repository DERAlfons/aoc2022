#include "mix.h"

#include <list>
#include <iterator>

using std::list;
using std::advance;

extern "C" {

int *mix(int *coords, int length, int rounds) {
    list<int> l = list<int>();
    list<int>::iterator *a = new list<int>::iterator[length];
    for (int i = 0; i < length; i++) {
        a[i] = l.insert(l.end(), coords[i]);
    }

    for (int r = 0; r < rounds; r++) {
        for (int i = 0; i < length; i++) {
            int dist = *a[i];

            list<int>::iterator it = a[i];
            for (int d = 0; d <= dist % (length - 1); d++) {
                advance(it, 1);
                if (it == l.end()) it = l.begin();
            }

            l.erase(a[i]);
            a[i] = l.insert(it, dist);
        }
    }

    int *result = new int[length];
    list<int>::iterator it = l.begin();
    for (int i = 0; i < length; i++) {
        result[i] = *it;
        advance(it, 1);
    }

    delete[] a;

    return result;
}

}