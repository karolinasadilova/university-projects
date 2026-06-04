#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


//fixni size tabulky:
#define tbl_size 15

struct node {
    char* content;//obsah
    bool deleted;//priznak
};
//hash fce:
unsigned int hash (char*key) {//hash fce vraci index~int pro ∀ key
    int len = strlen(key);//len stringu...n ve vzorci pro hash fci
    unsigned int ir = 5381 * pow(33, len);//intermediate result
    for (int i = 0; i<len; i++){//suma ve vzorci
        ir = ir + pow(33, len-i-1) * key[i];//exponent len-i...prvni i=0, prvni prubeh: bylo by to len-0 tj 33^n, ale ma byt 33^(n-1)
                                            //for cyklus by tedy "mohl|mel" zacinat od 1 ale indexuji od 0 kvuli nasobeni ascii hodnotou ∀ znaku -> exp musi byt (len-i-1)~prvni prubeh cyklu 33^n-1...posledni 33^len-1-(len-1)=33^0=33^(n-n)
    }                                         //i by mohlo zacinat na 1 p.k. :
                                                //         ir = ir + pow(33, len-i) * key[i-1]
                                                    //∀znak na indexu i je v c cislo...nasobeni key[i]~ASCII hodnota
    return ir%tbl_size;//index v intervalu <0,tbl_size-1> -> ir mod tbl_size("aby se vesel do tabulky")
}

//h2(k) = 1 + (k mod m− 1)
unsigned int h2 (char* key) {
    return 1 + (hash(key) % (tbl_size - 1));
}


//pruzkumna funkce~double hash:(reseni kolizi "kdyz je misto v poli/tabulce na indexu plne tak kam dal")
//g(k, i) = (h1(k) + ih2(k)) mod m...;h1 a h2 chapu jako zadanou hash fci, m=tbl_size, i=tries_count
unsigned int double_hash(char*key,int tries_count) {
   return (hash(key)+tries_count*(h2(key)))%tbl_size;
}

//insert fce:
bool insert(struct node *table[], char *key) {
    //do tabulky insertuji key: vypocitam index pomoci hash fce, najdu volne misto && kolize resim double hashem, pokud zadne volne misto neni vracim false
    for (int i = 0; i < tbl_size; i++) {
        int index = double_hash(key, i);
        //spocitam prvni index, pokud je pole na danem indexu prazdne, vlozim klic a return true,kdyz je pole na danem indexu jiz plne tak inkrementuji i tj tries_count a resim kolizi dvojitym hashem
        if (table[index]->content == NULL) {
            table[index]->content = key;
            table[index]->deleted = false; //deleted je priznak kvuli nasledne fci delete a search
            return true;
        }
    }
    return false; //pokud nevlozim klic~tabulka je plna
}

//search fce:
struct node *search(struct node *table[], char *key) {
    //predam tabulku a klic ktery hledam
    for (int i = 0; i < tbl_size; i++) {
        //postupne forcyklem prochazim tabulku
        if (table[double_hash(key, i)]->content == NULL && table[double_hash(key, i)]->deleted == false) {
            //pokud mi hashovaci fce vrati index, ja se podivam do tabulky na dany index: prvek se tam nenachazi && a misto je prazdne, tak cyklus konci a vraci null
            return NULL;
        } //obecne: koncim v pripade, ze narazim na prazdne misto
        if (table[double_hash(key,i)]->content != NULL && strcmp(table[double_hash(key, i)]->content, key )== 0) {
            //stringcompare porovnava obsah stringu a klice
            return table[double_hash(key, i)];
        } //pokud se na indexu ktery mi vrati hash fce nachazi uzel se stejnym obsahem jako klic, tak vratim uzel na danem indexu
        if (table[double_hash(key, i)]->deleted == true) {
            //pokud uzel na vracenem indexu neni prazdny && neshoduje se s klicem && jedna se o misto v tabulce s "priznakem" tak pokracuji v hledani
            continue;
        }
    }
    return NULL; //pokud projdu celou tabulku a nenajdu hledany klic vracim null
}
//delete fce:
void delete(struct node*table[], char*key) {//predam tabulku a klic
struct node* element = search(table,key);//nejdrive vyhledam hledany klic
    if (element == NULL) {//pokud ho nenajdu tak se nic nedeje
        return;
    }
    element->content=NULL;//jinak:vymazu obsah nodu tj nastavim content na null
    element->deleted=true;//nastavim priznak na true
}

//print table
void print_table(struct node*table[]) {
    for (int i=0;i<tbl_size;i++) {
        if (table[i]->content!= NULL){//pokud obsah uzlu na danem indexu neni prazdny tak printuji obsah
            printf("| %s |\n",table[i]->content);}//jinak printuji "empty"
        else {
            printf("| empty |\n");}
    }
}

//kolize
//fce na inkrementaci stringu
char* next_string(char*string) {
    int len = strlen(string);
    for (int i=len-1;i>=0;i--) {//postupne jdu od konce stringu po index 0
        if (string[i]<'z') {
                string[i]=string[i]+1;//pokud nemam posleni znak abecedy tj 'z' tak navysim znak tj a->b, b->c...
                return string;//a vratim dany string
        }
        string[i]='a';//pokud je na indexu 'z' tak index nastavim na 'a' kvuli preteceni z->a
    }
    string[len]='a';//forcyklus skoncil a prodlouzim string zz->aaa...tzn ∀ znak stringu byl 'z' -> ted jsou prenastaveny na 'a' && nasledne prodlouzim string o dalsi 'a'
    string[len+1]='\0';//znak na konci nastavim na prazdny znak
    return string;
}

void colliding_strings() {
    int size=100;
    char *table[size];
    for (int i=0;i<size;i++) {
        table[i] = NULL;//vynuluji tabulku
    }
    int matches=0;//pocet kolizi
    char key[100] = "a";
    while (matches<100) {//dokud nemam 100 kolizi provadim while cyklus
        int index = hash(key);
        if (table[index] == NULL) {//pokud je na danem indexu prazdne misto "vlozim" tam klic
            table[index] = strdup(key);//string duplicate kvuli prepisovani hodnoty klice v pameti kvuli funkci next_string ta totiz po kazdem pruchodu prepise key v pameti a prepsalo by mi to hodnoty v tabulce
                                        //potrebuji vytvorit duplikat a vlozit ho do tabulky, bez pouziti duplikace by pointry v tabulce ukazovaly na stejne misto v pameti jako volani fce next_string
                                        //tzn pracovat s kopii a ne presne menit hodnotu v datove strukture->next string by menil data v tabulce
                                         //fce strdup dynamicky alokuje pamet jedna se o ekvivalent pouziti strcpy a mallocu
        }
        else {
            printf("kolize:| %s | %s |\n",table[index],key);//doslo ke kolizi takze printuji
            matches++;//zvysuju pocet kolizi
        }
        next_string(key);//inkrementuji string pomoci next_string
    }

}
int main(void) {

struct node* table[tbl_size];
for (int i=0;i<tbl_size;i++) {
    table[i] = (struct node*)malloc(sizeof(struct node));//dynamicky alokuji pamet pro ∀ node
    table[i]->content=NULL;//nastavim content ∀ nodu na null
    table[i]->deleted=false;};//nastavim priznak na false jelikoz zatim zadny uzel nebyl odstranen


    insert(table,"way");
    insert(table,"hi");
    insert(table,"him");
    insert(table,"tea");
    insert(table,"hello");
    insert(table,"world");
    insert(table,"lorem");
    insert(table,"ipsum");
    printf("ipsum:%d\n",hash("ipsum"));
    printf("ipsum:%d\n",double_hash("ipsum",1));
    printf("ipsum:%d\n",double_hash("ipsum",2));
    printf("lorem:%d\n",hash("lorem"));

    print_table(table);

    bool s = search(table,"hi");
    printf("%d\n",s);

    delete(table,"hi");
    delete(table,"him");
    delete(table,"tea");
    delete(table,"hello");
    delete(table,"world");
    delete(table,"lorem");
    printf("\n");
    print_table(table);

    bool s2 = search(table,"hi");
    printf("%d\n",s2);
    colliding_strings();
    return 0;
}
