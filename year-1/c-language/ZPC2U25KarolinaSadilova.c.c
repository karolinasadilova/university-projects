#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <stdarg.h>

double kalkulator(const char *symbol, ...) {
    double zasobnik[20];
    int top = 0;

    va_list args;
    va_start(args, symbol);

    for (int i = 0; symbol[i] != '\0'; i++) {
        switch (symbol[i]) {
            case 'i':
                if (top < 20) {
                    zasobnik[top] = (double)va_arg(args, int);
                    top++;
                }
                break;

            case 'd':
                if (top < 20) {
                    zasobnik[top] = va_arg(args, double);
                    top++;
                }
                break;

            case '+':
                if (top >= 2) {
                    zasobnik[top - 2] = zasobnik[top - 2] + zasobnik[top - 1];
                    top--;
                }
                break;

            case '-':
                if (top >= 2) {
                    zasobnik[top - 2] = zasobnik[top - 2] - zasobnik[top - 1];
                    top--;
                }
                break;

            case '*':
                if (top >= 2) {
                    zasobnik[top - 2] = zasobnik[top - 2] * zasobnik[top - 1];
                    top--;
                }
                break;

            case '/':
                if (top >= 2) {
                    zasobnik[top - 2] = zasobnik[top - 2] / zasobnik[top - 1];
                    top--;
                }
                break;

            case 's':
                if (top >= 1) {
                    double v = zasobnik[top - 1];
                    zasobnik[top - 1] = v * v;
                }
                break;

            case 'r':
                if (top >= 1) {
                    zasobnik[top - 1] = sqrt(zasobnik[top - 1]);
                }
                break;
        }
    }

    va_end(args);

    return (top > 0) ? zasobnik[top - 1] : 0.0;
}

int main(void) {
    const char *E="ii+rdsis-*i/";
    double vysledek = kalkulator(E, 7, 2, 2.5, 4, 5);

    printf("Vysledek: %.2f\n", vysledek);

    return 0;
}
