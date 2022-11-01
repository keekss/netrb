sample_natural_graph_diameter <- function(
  choice = c('pg', 'mn', 'ir', 'wa', 'cr')
) {

  choice <- match.arg(choice)

  # Pre-calculated diameters to save on runtime.
  return(switch(
    choice,
    'pg' = 46,
    'mn' = 99,
    'ir' = 11,
    'wa' = 10,
    'cr' = 849,
    stop('Invalid choice: ', choice)
  ))
}
