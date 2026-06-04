//
//  main.c
//  ALGO2.02
//
//  Created by Karolína Sadilová on 17.02.2026.
//

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

int array_search (int A[], int x, int n)
{
    
    n = sizeof(A);
    for (int i = 0; i<n; i++) {
        if (A[i] == x){
            return i;
            
        }

    }
    return -1;
}


int binary_search(int A[ ],  int elem, int size){
    
    int p =size - 1;
    int l = 0;
    while (l < size) {
        int s = (int)floor ((l + p) / 2);
        if (A[s] == elem){
            return s;
            
        }
        if (elem < A[A[s]]){
            size = s - 1;
            
        }
        else{
            l = s + 1;
            
        }
    }
    return -1;
    
}

int interpolar_search (int A[ ], int size, int elem, int *steps){
   
    int p =size - 1;
    int l = 0;
    while (l < size) {
        ++*steps;
        int x = A[l];
        int  y= A[p];
        double  r = (elem - x) / (y - x);
        int s = (double)l + (p - l) * r;
        
        if (A[s] == elem){
            return s;
            
        }
        if (elem < A[A[s]]){
            size = s - 1;
            
        }
        else{
            l = s + 1;
            
        }
    }
    return -1;
    
}
          


int main(void) {
    int A[ ] = {1, 2 , 3, 4};
    int size = 4;
    int target = 4;
    int index = array_search(A, target, size);
    printf("%d\n", index);
    
    int indexb = binary_search(A, target, size);
    printf("%d\n", indexb);
    
    
    return 0;
}
