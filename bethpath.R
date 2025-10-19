library(igraph)
library(ggplot2)
library(ggraph)

# 1. Функция для создания случайных предпочтений избирателей
generate_preferences <- function(num_voters, candidates) {
  preferences <- list()
  for (i in 1:num_voters) {
    # Случайная перестановка кандидатов
    preferences[[i]] <- sample(candidates)
  }
  return(preferences)
}

# 2. Функция для построения матрицы побед (pairwise comparison matrix)
build_pairwise_matrix <- function(preferences, candidates) {
  n <- length(candidates)
  pairwise_matrix <- matrix(0, nrow = n, ncol = n)
  rownames(pairwise_matrix) <- candidates
  colnames(pairwise_matrix) <- candidates
  
  # Для каждого избирателя
  for (pref in preferences) {
    # Для каждой пары кандидатов
    for (i in 1:(length(pref)-1)) {
      for (j in (i+1):length(pref)) {
        winner <- pref[i]
        loser <- pref[j]
        pairwise_matrix[winner, loser] <- pairwise_matrix[winner, loser] + 1
      }
    }
  }
  
  return(pairwise_matrix)
}

# 3. Функция для вычисления силы пути (path strength)
compute_path_strength <- function(pairwise_matrix, candidates) {
  n <- length(candidates)
  strength_matrix <- matrix(0, nrow = n, ncol = n)
  rownames(strength_matrix) <- candidates
  colnames(strength_matrix) <- candidates
  
  # Инициализация: сила пути от i к j - это количество голосов, где i побеждает j
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        strength_matrix[i, j] <- pairwise_matrix[i, j]
      }
    }
  }
  
  # Алгоритм Флойда-Уоршелла для нахождения самых сильных путей
  for (k in 1:n) {
    for (i in 1:n) {
      if (i != k) {
        for (j in 1:n) {
          if (i != j && j != k) {
            strength_matrix[i, j] <- max(
              strength_matrix[i, j],
              min(strength_matrix[i, k], strength_matrix[k, j])
            )
          }
        }
      }
    }
  }
  
  return(strength_matrix)
}

# 4. Функция для определения победителя по методу beatpath
find_beatpath_winner <- function(strength_matrix, candidates) {
  n <- length(candidates)
  wins <- rep(0, n)
  names(wins) <- candidates
  
  # Кандидат A побеждает кандидата B, если сила пути от A к B больше, чем от B к A
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      cand_i <- candidates[i]
      cand_j <- candidates[j]
      
      if (strength_matrix[cand_i, cand_j] > strength_matrix[cand_j, cand_i]) {
        wins[cand_i] <- wins[cand_i] + 1
      } else if (strength_matrix[cand_j, cand_i] > strength_matrix[cand_i, cand_j]) {
        wins[cand_j] <- wins[cand_j] + 1
      }
      # В случае равенства - ничья, никто не получает очков
    }
  }
  
  # Победитель - кандидат с наибольшим количеством побед в попарных сравнениях
  max_wins <- max(wins)
  winners <- names(wins[wins == max_wins])
  
  return(list(winner = winners, scores = wins))
}

# 5. Функция для визуализации матрицы сил
plot_strength_matrix <- function(strength_matrix, pairwise_matrix) {
  # Создаем data frame для тепловой карты
  matrix_df <- expand.grid(
    from = rownames(strength_matrix),
    to = colnames(strength_matrix)
  )
  
  matrix_df$direct <- sapply(1:nrow(matrix_df), function(i) {
    pairwise_matrix[matrix_df$from[i], matrix_df$to[i]]
  })
  
  matrix_df$strength <- sapply(1:nrow(matrix_df), function(i) {
    strength_matrix[matrix_df$from[i], matrix_df$to[i]]
  })
  
  matrix_df$net_strength <- matrix_df$strength - 
    sapply(1:nrow(matrix_df), function(i) {
      strength_matrix[matrix_df$to[i], matrix_df$from[i]]
    })
  
  # Тепловая карта прямых сравнений
  p1 <- ggplot(matrix_df[matrix_df$from != matrix_df$to, ], 
               aes(x = to, y = from, fill = direct)) +
    geom_tile(color = "white") +
    geom_text(aes(label = direct), color = "white", size = 4) +
    scale_fill_gradient2(low = "#D73027", mid = "white", high = "#4575B4", 
                         midpoint = max(matrix_df$direct)/2) +
    labs(title = "Pairwise Comparison Matrix",
         subtitle = "Direct wins between candidates",
         x = "Loser", y = "Winner", fill = "Votes") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Тепловая карта сил путей
  p2 <- ggplot(matrix_df[matrix_df$from != matrix_df$to, ], 
               aes(x = to, y = from, fill = strength)) +
    geom_tile(color = "white") +
    geom_text(aes(label = strength), color = "white", size = 4) +
    scale_fill_gradient2(low = "#D73027", mid = "white", high = "#4575B4",
                         midpoint = max(matrix_df$strength)/2) +
    labs(title = "Beatpath Strength Matrix",
         subtitle = "Indirect path strengths between candidates",
         x = "To", y = "From", fill = "Strength") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p1)
  print(p2)
}

# 6. Продвинутая визуализация сети
plot_advanced_network <- function(strength_graph, pairwise_matrix, strength_matrix, candidates) {
  
  # Создаем data frame для узлов с дополнительной информацией
  nodes_df <- data.frame(
    name = candidates,
    direct_wins = sapply(candidates, function(cand) {
      sum(pairwise_matrix[cand, ] > pairwise_matrix[, cand])
    }),
    beatpath_wins = sapply(candidates, function(cand) {
      sum(sapply(candidates, function(other) {
        if (cand != other) {
          as.numeric(strength_matrix[cand, other] > strength_matrix[other, cand])
        } else 0
      }))
    }),
    total_votes = rowSums(pairwise_matrix)
  )
  
  # Создаем data frame для рёбер
  edges_df <- data.frame()
  for (i in 1:length(candidates)) {
    for (j in 1:length(candidates)) {
      if (i != j && strength_matrix[i, j] > strength_matrix[j, i]) {
        edges_df <- rbind(edges_df, data.frame(
          from = candidates[i],
          to = candidates[j],
          direct_margin = pairwise_matrix[i, j] - pairwise_matrix[j, i],
          path_strength = strength_matrix[i, j],
          label = paste0(pairwise_matrix[i, j], "-", pairwise_matrix[j, i])
        ))
      }
    }
  }
  
  # Создаем граф для ggraph
  g <- graph_from_data_frame(edges_df, vertices = nodes_df, directed = TRUE)
  
  # Визуализация с ggraph
  p <- ggraph(g, layout = 'linear', circular = TRUE) +
    geom_edge_arc(aes(edge_width = path_strength, 
                      edge_alpha = path_strength,
                      label = label),
                  arrow = arrow(length = unit(3, 'mm'), type = "closed"),
                  strength = 0.2,
                  check_overlap = TRUE,
                  label_colour = 'red',
                  label_size = 3,
                  end_cap = circle(8, 'mm')) +
    geom_node_point(aes(size = beatpath_wins, color = direct_wins), alpha = 0.8) +
    geom_node_text(aes(label = paste0(name, "\n", "BW:", beatpath_wins, "\n", "DW:", direct_wins)), 
                   repel = FALSE, size = 3) +
    scale_edge_width_continuous(range = c(0.5, 3)) +
    scale_edge_alpha_continuous(range = c(0.3, 1)) +
    scale_size_continuous(range = c(5, 15)) +
    scale_color_gradient(low = "lightblue", high = "darkblue") +
    labs(title = "Beatpath Method Network Visualization",
         subtitle = "Node size: Beatpath wins | Color: Direct wins | Edge width: Path strength",
         edge_width = "Path Strength",
         size = "Beatpath Wins",
         color = "Direct Wins") +
    theme_void() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))
  
  print(p)
  
  # Выводим таблицу с метриками
  cat("\nCandidate Metrics Summary:\n")
  cat("==========================\n")
  print(nodes_df[, c("name", "direct_wins", "beatpath_wins", "total_votes")])
}

# 7. Основная функция визуализации
visualize_beatpath <- function(results, candidates = c("A", "B", "C", "D", "E")) {
  
  pairwise_matrix <- results$pairwise_matrix
  strength_matrix <- results$strength_matrix
  
  # Создание графа для визуализации
  create_beatpath_graph <- function(matrix, matrix_type = "direct") {
    g <- graph.empty(n = length(candidates), directed = TRUE)
    V(g)$name <- candidates
    V(g)$label <- candidates
    V(g)$size <- 25
    V(g)$color <- "#4E79A7"
    V(g)$label.color <- "white"
    V(g)$label.cex <- 1.2
    
    # Добавление рёбер с весами
    edge_list <- c()
    edge_weights <- c()
    edge_labels <- c()
    
    for (i in 1:length(candidates)) {
      for (j in 1:length(candidates)) {
        if (i != j && matrix[i, j] > matrix[j, i]) {
          edge_list <- c(edge_list, candidates[i], candidates[j])
          edge_weights <- c(edge_weights, matrix[i, j] - matrix[j, i])
          edge_labels <- c(edge_labels, paste0(matrix[i, j], "-", matrix[j, i]))
        }
      }
    }
    
    if (length(edge_list) > 0) {
      g <- add_edges(g, edge_list, 
                     weight = edge_weights,
                     label = edge_labels,
                     color = if(matrix_type == "direct") "#59A14F" else "#EDC948",
                     width = sqrt(edge_weights)/2 + 1)
    }
    
    return(g)
  }
  
  # Создаем два графа: для прямых сравнений и для силы путей
  direct_graph <- create_beatpath_graph(pairwise_matrix, "direct")
  strength_graph <- create_beatpath_graph(strength_matrix, "strength")
  
  # Визуализация 1: Граф прямых сравнений
  par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))
  
  # Граф прямых побед
  plot(direct_graph, 
       main = "Pairwise Comparison Graph\n(Direct Wins)",
       layout = layout.circle,
       vertex.frame.color = "white",
       edge.arrow.size = 0.8,
       edge.curved = 0.2,
       edge.label = E(direct_graph)$label,
       edge.label.color = "darkgreen",
       edge.label.cex = 0.8)
  
  # Граф силы путей (beatpath)
  plot(strength_graph, 
       main = "Beatpath Strength Graph\n(Indirect Wins)",
       layout = layout.circle,
       vertex.frame.color = "white",
       edge.arrow.size = 0.8,
       edge.curved = 0.2,
       edge.label = E(strength_graph)$label,
       edge.label.color = "darkorange",
       edge.label.cex = 0.8)
  
  par(mfrow = c(1, 1))
  
  # Дополнительная визуализация: матрица сил путей
  plot_strength_matrix(strength_matrix, pairwise_matrix)
  
  # Визуализация с использованием ggraph (более современный подход)
  plot_advanced_network(strength_graph, pairwise_matrix, strength_matrix, candidates)
}

# 8. Анализ критических путей
analyze_critical_paths <- function(strength_matrix, pairwise_matrix, candidates) {
  cat("Critical Path Analysis:\n")
  for (i in 1:length(candidates)) {
    for (j in 1:length(candidates)) {
      if (i != j && strength_matrix[i, j] > pairwise_matrix[i, j]) {
        cat(sprintf("%s → %s: Direct: %d, Beatpath: %d (Improvement: +%d)\n",
                    candidates[i], candidates[j],
                    pairwise_matrix[i, j], strength_matrix[i, j],
                    strength_matrix[i, j] - pairwise_matrix[i, j]))
      }
    }
  }
}

# 9. ГЛАВНАЯ ФУНКЦИЯ - объединяет все компоненты
beatpath_method_complete <- function(num_voters = 45, candidates = c("A", "B", "C", "D", "E")) {
  cat("Beatpath Method (Schulze Method) Implementation\n")
  cat("=============================================\n")
  cat("Number of voters:", num_voters, "\n")
  cat("Candidates:", paste(candidates, collapse = ", "), "\n\n")
  
  # Генерация случайных предпочтений
  set.seed(123) # для воспроизводимости
  preferences <- generate_preferences(num_voters, candidates)
  
  cat("First 5 voters' preferences:\n")
  for (i in 1:min(5, length(preferences))) {
    cat("Voter", i, ":", paste(preferences[[i]], collapse = " > "), "\n")
  }
  cat("\n")
  
  # Построение матрицы попарных сравнений
  pairwise_matrix <- build_pairwise_matrix(preferences, candidates)
  
  cat("Pairwise Comparison Matrix:\n")
  print(pairwise_matrix)
  cat("\n")
  
  # Вычисление силы путей
  strength_matrix <- compute_path_strength(pairwise_matrix, candidates)
  
  cat("Path Strength Matrix:\n")
  print(strength_matrix)
  cat("\n")
  
  # Определение победителя
  result <- find_beatpath_winner(strength_matrix, candidates)
  
  cat("Results:\n")
  for (cand in candidates) {
    cat(cand, ":", result$scores[cand], "wins\n")
  }
  cat("\n")
  
  if (length(result$winner) == 1) {
    cat("Winner:", result$winner, "\n")
  } else {
    cat("Tie between:", paste(result$winner, collapse = ", "), "\n")
  }
  
  # Создаем объект с результатами
  results <- list(
    preferences = preferences,
    pairwise_matrix = pairwise_matrix,
    strength_matrix = strength_matrix,
    result = result
  )
  
  # Визуализация
  cat("\nGenerating visualizations...\n")
  visualize_beatpath(results, candidates)
  
  # Дополнительный анализ
  cat("\nDetailed Analysis:\n")
  cat("==================\n")
  
  # Покажем несколько сравнений между кандидатами
  for (i in 1:(length(candidates)-1)) {
    for (j in (i+1):length(candidates)) {
      cand1 <- candidates[i]
      cand2 <- candidates[j]
      cat(cand1, "vs", cand2, ":")
      cat(" Direct:", results$pairwise_matrix[cand1, cand2], "-", 
          results$pairwise_matrix[cand2, cand1])
      cat(" | Path strength:", results$strength_matrix[cand1, cand2], "-", 
          results$strength_matrix[cand2, cand1])
      
      if (results$strength_matrix[cand1, cand2] > results$strength_matrix[cand2, cand1]) {
        cat(" →", cand1, "wins\n")
      } else if (results$strength_matrix[cand2, cand1] > results$strength_matrix[cand1, cand2]) {
        cat(" →", cand2, "wins\n")
      } else {
        cat(" → Tie\n")
      }
    }
  }
  
  # Анализ критических путей
  cat("\n")
  analyze_critical_paths(strength_matrix, pairwise_matrix, candidates)
  
  return(results)
}

# 10. ЗАПУСК ПРОГРАММЫ
cat("Starting Beatpath Method Analysis...\n")
results <- beatpath_method_complete()

cat("\nAnalysis complete! Check the plots for visualizations.\n")
