#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    Muz=1,Zena
}Pohlavi;

typedef struct {
    char j[12]; Pohlavi p;
}Jmeno;
Jmeno J[134];
int count=0;

int exists(char *name) {
    for (int i=0;i<count;i++) {
        if (strcmp(name,J[i].j)==0) {
            return 1;
        }
    }
    return 0;
}

void load() {
    FILE *file = fopen("Jmena","rb");
    while (1) {
        int len;
        if (fread(&len, 1, 1, file) != 1) break;

        Pohlavi p;
        if (len >= 50) {
            p = Zena;
            len -= 50;
        } else {
            p = Muz;
        }
        char name[12];
        fread(name, 1, len, file);
        name[len] = '\0';

        if (!exists(name)) {
            strcpy(J[count].j, name);
            J[count].p = p;
            count++;
        }
    }
    fclose(file);
}
int cmp(const void *a, const void *b) {
    return strcmp(((Jmeno*)a)->j, ((Jmeno*)b)->j);
}

void printing(Pohlavi p) {
    int c = 0;
    for (int i = 0; i < count; i++) {
        if (J[i].p == p) {
            printf("%s ", J[i].j);
            c++;
            if (c % 10 == 0) printf("\n");
        }
    }
    printf("\n");
}

int main(void) {
    load();
    qsort(J, count, sizeof(Jmeno), cmp);
    printing(Zena);
    printf("\n");
    printing(Muz);

    return 0;
}
