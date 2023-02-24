#include "vis.h"

#include <stdlib.h>

int scenic_score(int *a, int height, int width, int posI, int posJ) {
    int (*trees)[width] = (int (*)[width])a;

    int tree_size = trees[posI][posJ];

    int topScore = 0;
    for (int i = posI - 1; i >= 0; i--) {
        topScore += 1;

        if (trees[i][posJ] >= tree_size) break;
    }

    int bottomScore = 0;
    for (int i = posI + 1; i < height; i++) {
        bottomScore += 1;

        if (trees[i][posJ] >= tree_size) break;
    }

    int leftScore = 0;
    for (int j = posJ - 1; j >= 0; j--) {
        leftScore += 1;

        if (trees[posI][j] >= tree_size) break;
    }

    int rightScore = 0;
    for (int j = posJ + 1; j < width; j++) {
        rightScore += 1;

        if (trees[posI][j] >= tree_size) break;
    }

    return topScore * bottomScore * leftScore * rightScore;
}

int max_scenic_score(int *a, int height, int width) {
    int (*trees)[width] = (int (*)[width])a;

    int result = 0;
    for (int i = 0; i < height; i++) {
    for (int j = 0; j < width; j++) {
        int ss = scenic_score(a, height, width, i, j);

        if (ss > result) {
            result = ss;
        }
    }}

    return result;
}

int vis_count(int *a, int height, int width) {
    int (*trees)[width] = (int (*)[width])a;
    int (*visible)[width] = (int (*)[width])malloc(height * width * sizeof(int));

    for (int i = 0; i < height; i++) {
    for (int j = 0; j < width; j++) {
        visible[i][j] = 0;
    }}

    for (int i = 0; i < height; i++) {
        int tree_size_left = -1;
        int tree_size_right = -1;

        for (int j = 0; j < width; j++) {
            if (trees[i][j] > tree_size_left) {
                visible[i][j] = 1;

                tree_size_left = trees[i][j];
            }

            if (trees[i][width - j - 1] > tree_size_right) {
                visible[i][width - j - 1] = 1;

                tree_size_right = trees[i][width - j - 1];
            }
        }
    }

    for (int j = 0; j < width; j++) {
        int tree_size_top = -1;
        int tree_size_bottom = -1;

        for (int i = 0; i < height; i++) {
            if (trees[i][j] > tree_size_top) {
                visible[i][j] = 1;

                tree_size_top = trees[i][j];
            }

            if (trees[height - i - 1][j] > tree_size_bottom) {
                visible[height - i - 1][j] = 1;

                tree_size_bottom = trees[height - i - 1][j];
            }
        }
    }

    int result = 0;
    for (int i = 0; i < height; i++) {
    for (int j = 0; j < width; j++) {
        result += visible[i][j];
    }}

    free(visible);

    return result;
}
