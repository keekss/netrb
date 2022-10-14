ensure_chunks <- function(rs, attr = 'rand') {
  require_class('simulator', rs, 'rs')

  .chunks_dir <- chunks_dir(rs = rs,
                            attr = attr)
  # If chunks are not written to file, write them.
  if (!(dir.exists(.chunks_dir))) {
    logf('Chunks not found at `%s`; writing to file.',
         .chunks_dir)
    write_chunks(rs = rs,
                 attr = attr)
  }
}
