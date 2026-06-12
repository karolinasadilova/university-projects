//
//  main.c
//  ZPC2U22KarolinaSadilova.c
//
//  Created by Karolína Sadilová on 17.02.2026.
//
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "Jmena2"



int main(void) {
    char J[127][12];
    int i = 0;
    int radek = -1;
    int sloupec = 0;
    
    while (Jmena[i] != '\0') {
        if (isupper(Jmena[i])) {
            if (radek >= 0) J[radek][sloupec] = '\0';
            radek++;
            sloupec = 0;
        }
        J[radek][sloupec] = Jmena[i];
        sloupec++;
        i++;
    }
    J[radek][sloupec] = '\0';
    int pocet_jmen = radek + 1;

 
    int max_delka;
    printf("Zadejte delku radku: ");
    scanf("%d", &max_delka);

    int aktualni_delka = 0;

    for (int n = 0; n < pocet_jmen; n++) {
        int delka_jmena = strlen(J[n]);

        if (aktualni_delka + delka_jmena > max_delka) {
            printf("\n");
            aktualni_delka = 0;
        }
        
        if (aktualni_delka > 0) {
            printf(" ");
            aktualni_delka++;
        }

        printf("%s", J[n]);
        aktualni_delka += delka_jmena;
    }

    printf("\n");
    return 0;
}
