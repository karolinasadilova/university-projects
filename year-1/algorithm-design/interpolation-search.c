#include <stdio.h>
#include <math.h>



    int interpolationSearch(int array[], int size, int value) {
        int high = size - 1;
        int low = 0;

        while(value >= array[low] && value <= array [high] && low <= high) {


            int s = floor (low + (high - low) * (value - array[low]) / (array[high] - array[low]));

            if (array[s] == value) {
                return s;
            }
            else if (array [s] < value){
                low = s + 1;
            }
            else{
                high = s - 1;
            }
        }
        return -1;
    }



    int main(void) {
        int array []= {1, 2, 3, 4, 5, 6, 7, 8, 9};
        int size = sizeof(array)/sizeof(array[0]);
        //sizeof vrací bajty musim to videlit bajtama jednoho prvku at mam pocet prvku v array
        int index = interpolationSearch(array, size, 4);
        if (index != -1) {
            printf("index of an element : %d\n", index);
        }
        else {
            printf("element is not in the array\n");
        }
        return 0;
    }
