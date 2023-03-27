#include "mix.h"

#include <list>
#include <iterator>

using std::list;
using std::advance;

extern "C" {

long long int *mix(long long int *coords, int length, int rounds) {
    list<long long int> l = list<long long int>();
    list<long long int>::iterator *a = new list<long long int>::iterator[length];
    for (int i = 0; i < length; i++) {
        a[i] = l.insert(l.end(), coords[i]);
    }

    for (int r = 0; r < rounds; r++) {
        for (int i = 0; i < length; i++) {
            long long int value = *a[i];
            long long int dist = ((value % (length - 1) + (length - 1)) % (length - 1));

            list<long long int>::iterator it = a[i];
            for (int d = 0; d <= dist; d++) {
                advance(it, 1);
                if (it == l.end()) it = l.begin();
            }

            l.erase(a[i]);
            a[i] = l.insert(it, value);
        }
    }

    long long int *result = new long long int[length];
    list<long long int>::iterator it = l.begin();
    for (int i = 0; i < length; i++) {
        result[i] = *it;
        advance(it, 1);
    }

    delete[] a;

    return result;
}

}
