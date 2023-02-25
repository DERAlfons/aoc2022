#include "lines_of_sight.h"

#include <stdlib.h>

int countVisibleTrees(int *a, int height, int width) {
    int (*trees)[width] = (int (*)[width])a;
    int (*visible)[width] = malloc(height * width * sizeof(int));

    for (int i = 0; i < height; i++) {
    for (int j = 0; j < width; j++) {
        visible[i][j] = 0;
    }}

    for (int i = 0; i < height; i++) {
        int treeSizeLeft = -1;
        int treeSizeRight = -1;

        for (int j = 0; j < width; j++) {
            if (trees[i][j] > treeSizeLeft) {
                visible[i][j] = 1;

                treeSizeLeft = trees[i][j];
            }

            if (trees[i][width - j - 1] > treeSizeRight) {
                visible[i][width - j - 1] = 1;

                treeSizeRight = trees[i][width - j - 1];
            }
        }
    }

    for (int j = 0; j < width; j++) {
        int treeSizeTop = -1;
        int treeSizeBottom = -1;

        for (int i = 0; i < height; i++) {
            if (trees[i][j] > treeSizeTop) {
                visible[i][j] = 1;

                treeSizeTop = trees[i][j];
            }

            if (trees[height - i - 1][j] > treeSizeBottom) {
                visible[height - i - 1][j] = 1;

                treeSizeBottom = trees[height - i - 1][j];
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

int getScenicScore(int *a, int height, int width, int posI, int posJ) {
    int (*trees)[width] = (int (*)[width])a;

    int treeSize = trees[posI][posJ];

    int topScore = 0;
    for (int i = posI - 1; i >= 0; i--) {
        topScore += 1;

        if (trees[i][posJ] >= treeSize) break;
    }

    int bottomScore = 0;
    for (int i = posI + 1; i < height; i++) {
        bottomScore += 1;

        if (trees[i][posJ] >= treeSize) break;
    }

    int leftScore = 0;
    for (int j = posJ - 1; j >= 0; j--) {
        leftScore += 1;

        if (trees[posI][j] >= treeSize) break;
    }

    int rightScore = 0;
    for (int j = posJ + 1; j < width; j++) {
        rightScore += 1;

        if (trees[posI][j] >= treeSize) break;
    }

    return topScore * bottomScore * leftScore * rightScore;
}

int getMaxScenicScore(int *a, int height, int width) {
    int result = 0;
    for (int i = 0; i < height; i++) {
    for (int j = 0; j < width; j++) {
        int ss = getScenicScore(a, height, width, i, j);

        if (ss > result) {
            result = ss;
        }
    }}

    return result;
}