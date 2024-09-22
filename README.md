# Huffman coding in Haskell, optimized for speed
This is my attempt to write some low level haskell to see how close in can get to
systems languages in terms of speed and memory consumption.

I am fairly happy withe result, i can compress 100MB in 0.51 seconds and decompress the result in in 0.79 seconds
with less than 100KB of resident memory. (as measured by GHCs runtime profiling)

This implementation is streaming by using lazy bytestrings (and lazy IO :O).
The (de)compression is performed in chunks, i don't do the chunking myself but simply use
`Data.ByteString.Lazy.toChunks` to expose the underlying strict bytestring chunks.

## Profiling output from compressing 100 MB of randomness:
```
     410,871,032 bytes allocated in the heap
         244,400 bytes copied during GC
          65,256 bytes maximum residency (2 sample(s))
          33,048 bytes maximum slop
              11 MiB total memory in use (0 MiB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0        93 colls,     0 par    0.001s   0.001s     0.0000s    0.0000s
  Gen  1         2 colls,     0 par    0.000s   0.000s     0.0002s    0.0002s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.513s  (  0.514s elapsed)
  GC      time    0.001s  (  0.001s elapsed)
  EXIT    time    0.000s  (  0.004s elapsed)
  Total   time    0.515s  (  0.520s elapsed)

  Alloc rate    800,386,380 bytes per MUT second

  Productivity  99.7% of total user, 98.9% of total elapsed
```

