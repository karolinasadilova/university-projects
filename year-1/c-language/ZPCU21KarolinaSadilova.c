//
//  main.c
//  ZPC2U21KarolinaSadilova.c
//
//  Created by Karolína Sadilová on 10.02.2026.
//
#include <stdio.h>
#include <string.h>
#include <ctype.h>

void Sifrovat (const char T[],const char K[],char S[]){
    size_t n = strlen(T);
    size_t m = strlen(K);
    
    for (int i = 0; i < n; i++) {
        if (islower(T[i])) {
            S[i] = 'a' + ((T[i] - 'a') + (K[i % m] - 'A')) % 26;
        }
            else {
                S[i] = T[i];
        }
    }
    S[n] ='\0';
}

void Desifrovat(const char S[], const char K[], char D[]) {
    size_t n = strlen(S);
    size_t m = strlen(K);
    
    for (int i = 0; i < n; i++) {
        if (islower(S[i])) {
            D[i] = 'a' + ((S[i] - 'a') + 26 - (K[i % m] - 'A')) % 26;
        } else {
            D[i] = S[i];
        }
    }
    D[n] = '\0';
}

int main(void) {
    char T[100];
    char K[100];
    char S[100];
    char D[100];
    
    
    printf("Zadejte text: ");
    scanf("%s", T);
    
    printf("Zadejte klic: ");
    scanf("%s", K);
    
    Sifrovat(T, K, S);
    printf("zasifrovany text: %s\n", S);

    Desifrovat(S, K, D);
    printf("desifrovany text: %s\n", D);
    
    return 0;
}
