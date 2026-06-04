//
//  main.c
//  ALGO2.03
//
//  Created by Karolína Sadilová on 24.02.2026.
//

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

struct node {
    int value;
    struct node*next;
};

typedef struct node node;
typedef node* list;

struct foo {
    int bar;
};

bool is_empty(list l) {
    return l == NULL;
}

int length(list l) {
    int count = 0;
    for (node *curr = l; curr != NULL; curr = curr->next) {
        count++;
    }
    return count;
}
void print_values(list l){
    for (node *curr = l; curr != NULL; curr = curr->next) {
        printf("%i\n", curr->value);
    }
}

bool element_at(list l, int index, int*value){
    
    int i = 0;
    for (node *curr = l; curr != NULL; curr = curr->next) {
        if (i == index) {
            *value = curr->value;
            return true;
        }
        i++;
    }
    return false;
}

void insert_head(list* l, node* n){

    n->next =*l;
    *l = n;
}

void insert_tail (list* l, node* n){
    
}
    

int main(void) {
    
    
    {
        node n1 = { .value = 4, .next = NULL};
        node n2 = { .value = 3, .next = &n1};
        node n3 = { .value= 2, .next = &n2 };
        node n4 = { .value = 1, .next = &n3};
        list l = &n4;
        
        struct foo x = { .bar = 4 };
        struct foo* x_ptr = &x;
        printf("%i\n", x_ptr->bar);
        
    }
    return 0;
}

