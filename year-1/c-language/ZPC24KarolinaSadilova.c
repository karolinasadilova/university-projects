//
//  main.c
//  ZPC24KarolinaSadilova.c
//
//  Created by Karolína Sadilová on 03.03.2026.
//

#include <stdlib.h>
#include <stdio.h>
#include "U24"



char vetsi(Datum a, Datum b){
    if (a.rok > b.rok) return 1;
    if (a.rok < b.rok) return 0;
    if (a.mesic > b.mesic) return 1;
    if (a.mesic < b.mesic) return 0;
    return (a.den > b.den);
};


char mensi(Datum a, Datum b){
    if (a.rok < b.rok) return 1;
    if (a.rok > b.rok) return 0;
    if (a.mesic < b.mesic) return 1;
    if (a.mesic > b.mesic) return 0;
    return (a.den < b.den);
    
};

void Tridit(Datum dates [],int n,char (*compare)(Datum,Datum)){
    
    
    for (int i = 0; i < n-1; i++) {
        for (int j = 0; j < n-1-i; j++) {
            if (compare (dates[j], dates[j+1])){
                Datum temp = dates[j];
                dates [j]= dates[j + 1];
                dates [j+1]= temp;
            
            }
        }
    }
}


void Vypsat(const Datum dates[],int n){
    for (int i = 0; i<n; i++) {
        printf("%d.%d.%d", dates[i].den, dates[i].mesic, dates[i].rok);
        
        if ((i + 1) % 5 == 0)
                    printf("\n");
                else
                    printf("  ");
    }
}

int main(void) {
    
    Tridit(D, POCET, (vetsi));
    Vypsat(D, POCET);
    printf("\n");
    printf("\n");
    Tridit(D, POCET, (mensi));
    Vypsat(D, POCET);
    return 0;
    
    
}
