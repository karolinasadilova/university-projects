#include <stdio.h>
#include "U23"


void Vyber(const int *vstupni_pole_ukazatelu[], int delka_A, Typ filtr, int vystupni_seznam[]) {
    int celkovy_pocet_unikatnich = 0;

    
    for (int i = 0; i < delka_A; i++) {
        const int *aktualni_podpole = vstupni_pole_ukazatelu[i];
        
       
        int pocet_cisel_v_podpoli = aktualni_podpole[0];

        
        for (int j = 1; j <= pocet_cisel_v_podpoli; j++) {
            int aktualni_cislo = aktualni_podpole[j];
            int splnuje_podminku = 0;

            
            if (filtr == VSECHNA) splnuje_podminku = 1;
            else if (filtr == KLADNA && aktualni_cislo > 0) splnuje_podminku = 1;
            else if (filtr == NEZAPORNA && aktualni_cislo >= 0) splnuje_podminku = 1;
            else if (filtr == ZAPORNA && aktualni_cislo < 0) splnuje_podminku = 1;

            if (splnuje_podminku) {
                
                int je_duplicitni = 0;
                for (int k = 1; k <= celkovy_pocet_unikatnich; k++) {
                    if (vystupni_seznam[k] == aktualni_cislo) {
                        je_duplicitni = 1;
                        break;
                    }
                }

                
                if (!je_duplicitni) {
                    celkovy_pocet_unikatnich++;
                    vystupni_seznam[celkovy_pocet_unikatnich] = aktualni_cislo;
                }
            }
        }
    }
    
    
    vystupni_seznam[0] = celkovy_pocet_unikatnich;
}

void Vypis(int data[]) {
    int pocet_prvku = data[0];

    for (int i = 1; i <= pocet_prvku; i++) {
        printf("%d ", data[i]);
        
        
        if (i % 10 == 0) {
            printf("\n");
        }
    }
    
   
    if (pocet_prvku % 10 != 0) {
        printf("\n");
    }
}

#include "U23T"

int main(void) {
    T();
    return 0;
}
