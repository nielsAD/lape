const
  RESOLUTION_DEFAULT = 2.0;
  RANDOM_SEED = 101010;

  { * Defaults for small cache size problems * }
  FFT_SIZE = 1024;
  SOR_SIZE = 100;
  SPARSE_SIZE_M = 1000;
  SPARSE_SIZE_nz = 5000;
  LU_SIZE = 100;

  { * large (out-of-cache) problem sizes * }
  LG_FFT_SIZE = 1024 * 1024;
  LG_SOR_SIZE = 1000;
  LG_SPARSE_SIZE_M = 100000;
  LG_SPARSE_SIZE_nz = 1000000;
  LG_LU_SIZE = 1000;

  { * Huge sets for modern CPUs with large caches * }
  HG_FFT_SIZE = 1024 * 1024 * 16;
  HG_SOR_SIZE = 10000;
  HG_SPARSE_SIZE_M = 1000000;
  HG_SPARSE_SIZE_nz = 10000000;
  HG_LU_SIZE = 100000;

  { *Tiny size, used to warm up the JIT. Probably unused in Delphi * }
  TINY_FFT_SIZE = 16;
  TINY_SOR_SIZE = 10;
  TINY_SPARSE_SIZE_M = 10;
  TINY_SPARSE_SIZE_N = 10;
  TINY_SPARSE_SIZE_nz = 50;
  TINY_LU_SIZE = 10;