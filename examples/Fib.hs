fib = fib' 0 1

fib' = \a b i ->
  case i of
    0 -> a
    _ ->
      let b' = Natural.add a b
          i' = Natural.sub i 1
      in fib' b b' i'