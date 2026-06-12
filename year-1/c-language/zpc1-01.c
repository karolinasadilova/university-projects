//
//  main.c
//  ZPU1
//
//



#include <assert.h>
#include <stdio.h>



// Porovnani 2 slov
int porovnani_slov(char *src, int i, char *words, int j) {
    while (src[i] != '\0' && src[i] != ' ' &&
           words[j] != '\0' && words[j] != ' ') {
        if (src[i] != words[j]) return 0;
        i++;
        j++;
    }
    // Slovo musi skoncit zaroven
    return (src[i] == '\0' || src[i] == ' ') &&
           (words[j] == '\0' || words[j] == ' ');
}

// Slovo ze src je v seznamu words
int obsahuje_slovo(char *src, int zacatek, int konec, char *words) {
    int j = 0;
    while (words[j] != '\0') {
        while (words[j] == ' ') j++;
        int start = j;
        while (words[j] != '\0' && words[j] != ' ') j++;
        if (porovnani_slov(src, zacatek, words, start)) return 1;
    }
    return 0;
}

// funkce delete_words
int delete_words(char *src, char *words) {
    int i = 0, zapis = 0, smazano = 0;
    int prvni = 1;

    while (src[i] != '\0') {
        while (src[i] == ' ') i++;
        if (src[i] == '\0') break;

        int zacatek = i;
        while (src[i] != '\0' && src[i] != ' ') i++;
        int konec = i;

        if (obsahuje_slovo(src, zacatek, konec, words)) {
            smazano++;
        } else {
            if (!prvni) src[zapis++] = ' ';
            for (int j = zacatek; j < konec; j++) {
                src[zapis++] = src[j];
            }
            prvni = 0;
        }
    }

    src[zapis] = '\0';
    return smazano;
}


