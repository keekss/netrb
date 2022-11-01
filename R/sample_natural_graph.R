sample_natural_graph <- function(
  choice   = c('pg', 'mn', 'ir', 'wa', 'cr'),
  root_dir = sprintf('%s/networks', getwd())) {

  if (!(dir.exists(root_dir))) stop(
    'Invalid root directory for network files.'
  )

  choice <- match.arg(choice)

  file_name <- switch(
    choice,
    'pg' = 'power-grid.txt',
    'mn' = 'roads-minnesota.txt',
    'ir' = 'internet-routers.txt',
    'wa' = 'eurosis-webatlas.graphml',
    'ca' = 'roads-california.txt',
    stop('Invalid choice: ', choice)
  )

  .path <- sprintf('%s/%s', root_dir, file_name)

  format   <- if (choice == 'wa') 'graphml' else 'edgelist'
  directed <- if (choice == 'wa') TRUE      else FALSE

  return(read_graph(
    file     = .path,
    format   = format,
    directed = directed
  ))
}
