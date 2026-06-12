#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define VYSKA 30
#define SIRKA 80

int main() {
    char plocha[VYSKA][SIRKA];
    memset(plocha, ' ', sizeof(plocha));

    short pozX = 0, pozY = 0;

    FILE* soubor = fopen("Kresba", "rb");
    if (!soubor) {
        perror("error");
        return 1;
    }

    while (1) {
        int prikaz = fgetc(soubor);
        if (prikaz == EOF) break;

        switch (prikaz) {
        case 1: {
            if (fread(&pozX, sizeof(short), 1, soubor) != 1) break;
            if (fread(&pozY, sizeof(short), 1, soubor) != 1) break;
            break;
        }
        case 2: {
            short posunX, posunY;
            if (fread(&posunX, sizeof(short), 1, soubor) != 1) break;
            if (fread(&posunY, sizeof(short), 1, soubor) != 1) break;
            pozX += posunX;
            pozY += posunY;
            break;
        }
        case 3: {
            unsigned char znak;
            short smerX, smerY, delka;
            if (fread(&znak, sizeof(char), 1, soubor) != 1) break;
            if (fread(&smerX, sizeof(short), 1, soubor) != 1) break;
            if (fread(&smerY, sizeof(short), 1, soubor) != 1) break;
            if (fread(&delka, sizeof(short), 1, soubor) != 1) break;

            for (int i = 0; i < delka; i++) {
                if (pozX >= 0 && pozX < SIRKA && pozY >= 0 && pozY < VYSKA)
                    plocha[pozY][pozX] = znak;
                pozX += smerX;
                pozY += smerY;
            }
            break;
        }
        case 4: {
            short krokX, krokY, pocet;
            if (fread(&krokX, sizeof(short), 1, soubor) != 1) break;
            if (fread(&krokY, sizeof(short), 1, soubor) != 1) break;
            if (fread(&pocet, sizeof(short), 1, soubor) != 1) break;

            char* text = malloc(pocet);
            if (!text) {
                perror("error");
                fclose(soubor);
                return 1;
            }

            if (fread(text, 1, pocet, soubor) != pocet) {
                free(text);
                break;
            }

            for (int i = 0; i < pocet; i++) {
                if (pozX >= 0 && pozX < SIRKA && pozY >= 0 && pozY < VYSKA)
                    plocha[pozY][pozX] = text[i];
                pozX += krokX;
                pozY += krokY;
            }
            free(text);
            break;
        }
        default:
            fprintf(stderr, "error: %d\n", prikaz);
            break;
        }
    }

    fclose(soubor);

    for (int r = 0; r < VYSKA; r++) {
        for (int s = 0; s < SIRKA; s++) {
            putchar(plocha[r][s]);
        }
        putchar('\n');
    }

    return 0;
}