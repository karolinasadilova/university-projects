//
//  main.c
//  algo2.01
//
//  Created by Karolína Sadilová on 10.02.2026.
//

#include <stdio.h>
#include <stdlib.h>

#include <math.h>

struct point
{
    int x_coord;
    int y_coord;
};
int x_coordinate(struct point a){
    return a.x_coord;
}

int y_coordinate(struct point a){
    return a.y_coord;
}

void print_point(struct point a){
    
    printf("(%d , %d)", x_coordinate(a), y_coordinate(a));
}
struct point closest_to_x(struct point points[], int n){
    int min_point = 0;
    for (int i = 1; i < n ; i++) {
        if (abs (points[i].y_coord) < (abs (points [min_point].y_coord)))
        {
            min_point = i;
        }
    }
    return points [min_point];
    
    
}

double distance(struct point a, struct point b){
    double x = a.x_coord - b.x_coord;
    double y = a.y_coord - b.y_coord;
    return sqrt(x * x + y * y);
}


double width(struct point points[], int n){
    double max_distance = 0.0;
    for (int i = 0.0; i<n; i++) {
        for (int j = i+1; j<n; j++) {
            if (distance(points[i], points[j]) > max_distance) {
                max_distance = distance(points[i], points[j]);
            }
        }
    }
    return max_distance;
}
int main(void) {
    struct point x = {6, 3};
    print_point(x);
    
    return 0;
}
