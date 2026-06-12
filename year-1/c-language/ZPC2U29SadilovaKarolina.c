#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "U29"





bool jePrestupny(int rok){
    if (rok % 400 == 0) return true;
    if (rok % 100 == 0) return false;
    if (rok % 4 == 0) return true;
    return false;
}

int najdi_mesic(const char *text)
{
    for (int i = 0; i < 24; i++) {
        if (strcmp(text, mesice[i]) == 0) {
            return i / 2 + 1;
        }
    }
    return 0;
}

int main(void) {
    FILE *f = fopen("Datum.txt", "r");

    if (f==NULL) return -1;

    char radek[256];

    while (fgets(radek, sizeof(radek), f) != NULL) {
        int den;
        int mesic;
        int rok;
        char textMesic[50];
        int chyba = 0;
        int prestupnyChyba = 0;
        int pozice;

        radek[strcspn(radek, "\r\n")] = '\0';



        if (sscanf(radek, "%d. %d. %d%n", &den, &mesic, &rok, &pozice) == 3 && radek[pozice] == '\0') {
            if (mesic < 1 || mesic > 12) {
                chyba = 1;
            }
            else if (den < 1 || den > dny[mesic - 1]) {
                chyba = 1;
            }
            else if (mesic == 2 && den == 29 && !jePrestupny(rok)) {
                prestupnyChyba = 1;
            }
        }
        else if (sscanf(radek, "%d. %49s %d%n", &den, textMesic, &rok, &pozice) == 3 && radek[pozice] == '\0') {
            mesic = najdi_mesic(textMesic);

            if (mesic == 0) {
                chyba = 1;
            }
            else if (den < 1 || den > dny[mesic - 1]) {
                chyba = 1;
            }
            else if (mesic == 2 && den == 29 && !jePrestupny(rok)) {
                prestupnyChyba = 1;
            }
        }
        else {
            chyba = 1;
        }

        if (chyba) {
            printf("%s chybny den nebo mesic\n", radek);
        }
        else if (prestupnyChyba) {
            printf("%s neni prestupny rok\n", radek);
        }
    }

    fclose(f);
    return 0;


}