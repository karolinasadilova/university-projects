//
//  main.c
//  Algo2.04
//
//  Created by Karolína Sadilová on 03.03.2026.
//

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

struct node{
    int value;
    struct node* left;
    struct node* right;
};

typedef struct node node;

void insert(node** t, int value);
void in_order_print(node* t);
void print_tree(node* t);
int height(node* t);

bool is_empty(node* t){
    if (t == NULL) {
        return true;
    }
    return false;
};

int max (int a, int b){
    return (a > b) ? a : b ;
}

int find_path (node* t, int result){
    node *tree = t;
    
    if (is_empty(tree)) {
        return result;
    }
    return max(find_path(tree->left, result + 1), find_path(tree->right, result + 1));
}

int height(node* t){
    int tree_height = find_path(t, -1);
    return tree_height;
}
//verze2:

int height2(struct node *node){
    if (node == NULL) {
        return 0;
    }
    else
    {
        int leftheight = height2(node->left);
        int rightheight = height2(node->right);
        
        if (leftheight > rightheight) {
            return (leftheight + 1);
        }
        else return (rightheight + 1);
        
    }
}

void in_order_print(node* t){
    if (t == NULL){
        return;
    }
    else
    {
        in_order_print(t->left);
        printf("%d ", t->value);
        in_order_print(t->right);
    }
}

void print_tree(node* t){
    if (t == NULL) {
        printf("-");
        return;
    }
    printf("(");
    printf("%d", t->value);
    print_tree(t->left);
    print_tree(t->right);
    printf(")");
}

node* min_value_node(node* t){
    if (t == NULL){
        return NULL;
    }
    else{
        while (t->left != NULL) {
            t = t->left;
        }
    }
    return t;
}

node* new_node(int value){
    node* n = malloc(sizeof(node));
    // malloc returns a pointer to newly allocated node n
    if (n){
        n->value = value;
        n->left = NULL;
        n->right = NULL;
    }
    
    return n;
}

//-> ekvivalent (*n).value = value

void insert(node** t, int value){
    node* newNode = new_node(value);
    

    if (*t == NULL) {
        *t = newNode;
        return;
    }

    node* current = *t;
    node* parent = NULL;
    while (current != NULL) {
        parent = current;
        if (value < current->value) {
            current = current->left;
        } else {
            current = current->right;
        }
    }

    if (value < parent->value) {
        parent->left = newNode;
    } else {
        parent->right = newNode;
    }
}

void fill(node** t, int n){
    for (int i = 0; i < n; i++) {
        int randomvalue = rand();
        insert(t, randomvalue);
    }
}


void rotate_left(node** t){
    
    node* current_tree = *t;
    node* subtree_x = current_tree;
    node* subtree_y = current_tree->left;
    node* subtree_b = subtree_y->left;
    node* tmp = subtree_y;
    
    subtree_y = subtree_x;
    subtree_x->right = subtree_b;
    subtree_x = tmp;
}




void rotate_right(node** t){
    
    
    
    
    node * temp = (*t)->left;
    (*t)->left = (* t)->left->right;
    temp->right = *t;
    *t = temp;
    
    
    node* current_tree = *t;
    node* subtree_x = current_tree;
    node* subtree_y = current_tree->left;
    node* subtree_b = subtree_y->left;
    node* tmp = subtree_x;
    
    subtree_x = subtree_y;
    subtree_x->left = subtree_b;
    subtree_y->right= tmp;
}
int main(void) {
    node n9 = { .value = 20, .left = NULL, .right = NULL };
    node n8 = { .value = 1, .left = NULL, .right = NULL };
    node n7 = { .value = 4, .left = NULL, .right = NULL };
    node n6 = { .value = 5, .left = NULL, .right = NULL };
    node n5 = { .value = 18, .left = NULL, .right = NULL };
    node n4 = { .value = 8 , .left = &n8, .right = &n9};
    node n3 = { .value = 14, .left = &n6, .right = &n7 };
    node n2 = { .value = 10, .left = &n4, .right = &n5 };
    node n1 = { .value = 12, .left = &n2, .right = &n3};

    // Use functions to avoid unused warnings
    (void)min_value_node;
    (void)height2;

    printf("In-order: ");
    in_order_print(&n1);
    printf("\nHeight: %d\n", height(&n1));

    // Build a dynamic BST and print it
    node* root = NULL;
    insert(&root, 10);
    insert(&root, 5);
    insert(&root, 15);
    insert(&root, 7);
    printf("BST in-order: ");
    in_order_print(root);
    printf("\n");

    return 0;
}
