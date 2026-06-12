//
//  main.c
//  hw3
//
//

#include <stdio.h>

int equivalence(int r[], int m) {
    // reflexivita:
    for (int i = 0; i < m; i++) {
        if (r[i * m + i] != 1)
        return 0;
    }
    // symetrie:
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < m; j++) {
            if (r[i * m + j] != r[j * m + i])
            return 0;
        }
    }
    // tranzitivita:
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < m; j++) {
            if (r[i * m + j]) {
                for (int k = 0; k < m; k++) {
                    if (r[j * m + k] && !r[i * m + k]) return 0;
                }
            }
        }
    }
    return 1;
}
/////////////////////////////////////////////////////////////

void transitive_closure(int r[], int m) {
    int changed = 1;

    while (changed) {
        changed = 0;

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < m; j++) {
                if (r[i*m + j]) {
                    for (int k = 0; k < m; k++) {
                        if (r[j*m + k] && !r[i*m + k]) {
                            r[i*m + k] = 1;
                            changed = 1;
                        }
                    }
                }
            }
        }
    }
}
int main (void){
    return 0;
}
