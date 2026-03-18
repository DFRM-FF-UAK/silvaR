# Przykładowe dane
plot_id = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)
tree_id = c(1, 2 ,3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 7, 8)
species = c('SO', 'SO', 'DB', 'BRZ', 'BK', 'DB', 'DB',
            'ŚW', 'ŚW', 'ŚW', 'SO', 'SO', 'SO', 'SO')
age = c(40, 40, 40, 40, 60, 45, 50, 50, 50, 60, 60, 60, 60, 60)
layer = c(1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1)
height = c(21, NA, 24, 20, 12, 20, 18, 20, NA, 31, 32, NA, 25, 20)
dbh = c(33, 32, 31, 40, 28, 47, 12, 22, 41, 48, 27, 42, 25, 33)

test_that("h_tree oblicza poprawne wartości wysokości dla use_measured_height = T", {
  result <- h_tree(plot_id = plot_id, tree_id = tree_id, species = species, age = age,
                   layer = layer, height = height, dbh = dbh, use_measured_height = T)
  expected_result <- c(21,20.79448,24,20,12,20,18,20,26.10109,31,32,27.42935,25,20)
  expect_equal(result, expected_result, tolerance = 1e-5)
})

test_that("h_tree oblicza poprawne wartości wysokości dla use_measured_height = T", {
  result <- h_tree(plot_id = plot_id, tree_id = tree_id, species = species, age = age,
                   layer = layer, height = height, dbh = dbh, use_measured_height = F)
  expected_result <- c(21, 20.79448, 24, 20, 12, 20, 18, 20,
                       26.10109, 31, 24.44369, 27.42935, 23.84522, 25.89575)
  expect_equal(result, expected_result, tolerance = 1e-5)
})


test_that("h_tree zgłasza ostrzeżenie dla gatunków bez parametrów", {
  expect_warning(h_tree(plot_id = plot_id, tree_id = tree_id,
                        species = c('SO', 'SO', 'DB', 'BRZ', 'BK', 'DB', 'DB',
                                    'ŚW', 'ŚW', 'ŚW', 'SO', 'SO', 'SO', NA),
                        age = age, layer = layer, height = height, dbh = dbh))
})
