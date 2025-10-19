#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MAX_CANDIDATES 10
#define MAX_VOTERS 100

// Структура для хранения результатов
typedef struct {
    char candidates[MAX_CANDIDATES];
    int num_candidates;
    int num_voters;
    int pairwise[MAX_CANDIDATES][MAX_CANDIDATES];
    int strength[MAX_CANDIDATES][MAX_CANDIDATES];
    int wins[MAX_CANDIDATES];
    char winner;
} ElectionResult;

// Функция для генерации случайных предпочтений
void generate_preferences(int num_voters, int num_candidates, 
                         int preferences[][MAX_CANDIDATES]) {
    for (int i = 0; i < num_voters; i++) {
        // Создаем массив кандидатов
        int cands[MAX_CANDIDATES];
        for (int j = 0; j < num_candidates; j++) {
            cands[j] = j;
        }
        
        // Перемешиваем кандидатов (Fisher-Yates shuffle)
        for (int j = num_candidates - 1; j > 0; j--) {
            int k = rand() % (j + 1);
            int temp = cands[j];
            cands[j] = cands[k];
            cands[k] = temp;
        }
        
        // Копируем в предпочтения
        for (int j = 0; j < num_candidates; j++) {
            preferences[i][j] = cands[j];
        }
    }
}

// Функция для построения матрицы попарных сравнений
void build_pairwise_matrix(int num_voters, int num_candidates,
                          int preferences[][MAX_CANDIDATES],
                          int pairwise[][MAX_CANDIDATES]) {
    // Инициализация матрицы нулями
    for (int i = 0; i < num_candidates; i++) {
        for (int j = 0; j < num_candidates; j++) {
            pairwise[i][j] = 0;
        }
    }
    
    // Для каждого избирателя
    for (int v = 0; v < num_voters; v++) {
        // Для каждой пары кандидатов
        for (int i = 0; i < num_candidates - 1; i++) {
            for (int j = i + 1; j < num_candidates; j++) {
                int winner = preferences[v][i];
                int loser = preferences[v][j];
                pairwise[winner][loser]++;
            }
        }
    }
}

// Функция для вычисления силы путей
void compute_path_strength(int num_candidates, 
                          int pairwise[][MAX_CANDIDATES],
                          int strength[][MAX_CANDIDATES]) {
    // Инициализация матрицы силы
    for (int i = 0; i < num_candidates; i++) {
        for (int j = 0; j < num_candidates; j++) {
            if (i != j) {
                strength[i][j] = pairwise[i][j];
            } else {
                strength[i][j] = 0;
            }
        }
    }
    
    // Алгоритм Флойда-Уоршелла для нахождения самых сильных путей
    for (int k = 0; k < num_candidates; k++) {
        for (int i = 0; i < num_candidates; i++) {
            if (i != k) {
                for (int j = 0; j < num_candidates; j++) {
                    if (i != j && j != k) {
                        int min_strength = (strength[i][k] < strength[k][j]) ? 
                                          strength[i][k] : strength[k][j];
                        if (min_strength > strength[i][j]) {
                            strength[i][j] = min_strength;
                        }
                    }
                }
            }
        }
    }
}

// Функция для определения победителя по методу beatpath
char find_beatpath_winner(int num_candidates, char candidates[],
                         int strength[][MAX_CANDIDATES],
                         int wins[]) {
    // Инициализация счетчиков побед
    for (int i = 0; i < num_candidates; i++) {
        wins[i] = 0;
    }
    
    // Попарные сравнения кандидатов
    for (int i = 0; i < num_candidates - 1; i++) {
        for (int j = i + 1; j < num_candidates; j++) {
            if (strength[i][j] > strength[j][i]) {
                wins[i]++;
            } else if (strength[j][i] > strength[i][j]) {
                wins[j]++;
            }
            // При равенстве - ничья, никто не получает очков
        }
    }
    
    // Поиск кандидата с максимальным количеством побед
    int max_wins = -1;
    char winner = candidates[0];
    int winner_count = 0;
    
    for (int i = 0; i < num_candidates; i++) {
        if (wins[i] > max_wins) {
            max_wins = wins[i];
            winner = candidates[i];
            winner_count = 1;
        } else if (wins[i] == max_wins) {
            winner_count++;
        }
    }
    
    // Если ничья, возвращаем первого из победителей
    return winner;
}

// Функция для вывода матрицы
void print_matrix(int num_candidates, char candidates[], 
                 int matrix[][MAX_CANDIDATES], const char* title) {
    printf("\n%s:\n", title);
    printf("   ");
    for (int i = 0; i < num_candidates; i++) {
        printf("%c  ", candidates[i]);
    }
    printf("\n");
    
    for (int i = 0; i < num_candidates; i++) {
        printf("%c  ", candidates[i]);
        for (int j = 0; j < num_candidates; j++) {
            printf("%2d ", matrix[i][j]);
        }
        printf("\n");
    }
}

// Функция для вывода предпочтений избирателей
void print_preferences(int num_voters, int num_candidates, 
                      char candidates[], int preferences[][MAX_CANDIDATES]) {
    printf("First 5 voters' preferences:\n");
    int max_to_show = (num_voters < 5) ? num_voters : 5;
    
    for (int i = 0; i < max_to_show; i++) {
        printf("Voter %d: ", i + 1);
        for (int j = 0; j < num_candidates; j++) {
            printf("%c", candidates[preferences[i][j]]);
            if (j < num_candidates - 1) {
                printf(" > ");
            }
        }
        printf("\n");
    }
}

// Основная функция метода Beatpath
ElectionResult beatpath_method(int num_voters, char candidates[], int num_candidates) {
    ElectionResult result;
    result.num_voters = num_voters;
    result.num_candidates = num_candidates;
    strcpy(result.candidates, candidates);
    
    // Инициализация генератора случайных чисел
    srand(time(NULL));
    
    printf("Beatpath Method (Schulze Method) Implementation in C\n");
    printf("===================================================\n");
    printf("Number of voters: %d\n", num_voters);
    printf("Candidates: ");
    for (int i = 0; i < num_candidates; i++) {
        printf("%c ", candidates[i]);
    }
    printf("\n");
    
    // Генерация случайных предпочтений
    int preferences[MAX_VOTERS][MAX_CANDIDATES];
    generate_preferences(num_voters, num_candidates, preferences);
    
    // Вывод предпочтений
    print_preferences(num_voters, num_candidates, candidates, preferences);
    
    // Построение матрицы попарных сравнений
    build_pairwise_matrix(num_voters, num_candidates, preferences, result.pairwise);
    print_matrix(num_candidates, candidates, result.pairwise, "Pairwise Comparison Matrix");
    
    // Вычисление силы путей
    compute_path_strength(num_candidates, result.pairwise, result.strength);
    print_matrix(num_candidates, candidates, result.strength, "Path Strength Matrix");
    
    // Определение победителя
    result.winner = find_beatpath_winner(num_candidates, candidates, 
                                       result.strength, result.wins);
    
    // Вывод результатов
    printf("\nResults:\n");
    for (int i = 0; i < num_candidates; i++) {
        printf("%c : %d wins\n", candidates[i], result.wins[i]);
    }
    printf("\nWinner: %c\n", result.winner);
    
    // Детальный анализ
    printf("\nDetailed Analysis:\n");
    printf("==================\n");
    
    for (int i = 0; i < num_candidates - 1; i++) {
        for (int j = i + 1; j < num_candidates; j++) {
            printf("%c vs %c: ", candidates[i], candidates[j]);
            printf("Direct: %d-%d", result.pairwise[i][j], result.pairwise[j][i]);
            printf(" | Path strength: %d-%d", result.strength[i][j], result.strength[j][i]);
            
            if (result.strength[i][j] > result.strength[j][i]) {
                printf(" → %c wins\n", candidates[i]);
            } else if (result.strength[j][i] > result.strength[i][j]) {
                printf(" → %c wins\n", candidates[j]);
            } else {
                printf(" → Tie\n");
            }
        }
    }
    
    // Анализ критических путей
    printf("\nCritical Path Analysis:\n");
    for (int i = 0; i < num_candidates; i++) {
        for (int j = 0; j < num_candidates; j++) {
            if (i != j && result.strength[i][j] > result.pairwise[i][j]) {
                printf("%c → %c: Direct: %d, Beatpath: %d (Improvement: +%d)\n",
                       candidates[i], candidates[j],
                       result.pairwise[i][j], result.strength[i][j],
                       result.strength[i][j] - result.pairwise[i][j]);
            }
        }
    }
    
    return result;
}

// Функция для тестирования
void run_test() {
    printf("=== BEATPATH METHOD TEST ===\n");
    
    // Параметры выборов
    int num_voters = 45;
    char candidates[] = {'A', 'B', 'C', 'D', 'E'};
    int num_candidates = 5;
    
    // Запуск метода
    ElectionResult result = beatpath_method(num_voters, candidates, num_candidates);
    
    printf("\n=== TEST COMPLETE ===\n");
}

int main() {
    run_test();
    return 0;
}

// Дополнительные утилиты

// Функция для сохранения результатов в файл
void save_results_to_file(ElectionResult result, const char* filename) {
    FILE* file = fopen(filename, "w");
    if (file == NULL) {
        printf("Error opening file for writing.\n");
        return;
    }
    
    fprintf(file, "Beatpath Method Results\n");
    fprintf(file, "=======================\n");
    fprintf(file, "Voters: %d\n", result.num_voters);
    fprintf(file, "Candidates: %s\n", result.candidates);
    fprintf(file, "Winner: %c\n", result.winner);
    
    fprintf(file, "\nPairwise Matrix:\n");
    for (int i = 0; i < result.num_candidates; i++) {
        fprintf(file, "%c ", result.candidates[i]);
        for (int j = 0; j < result.num_candidates; j++) {
            fprintf(file, "%d ", result.pairwise[i][j]);
        }
        fprintf(file, "\n");
    }
    
    fclose(file);
    printf("Results saved to %s\n", filename);
}

// Функция для загрузки предпочтений из файла (альтернатива случайной генерации)
void load_preferences_from_file(const char* filename, 
                               int preferences[][MAX_CANDIDATES],
                               int* num_voters, int* num_candidates) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        printf("Error opening preferences file.\n");
        return;
    }
    
    // Простая реализация - можно расширить под конкретный формат файла
    printf("File loading functionality - implement based on your file format.\n");
    
    fclose(file);
}
