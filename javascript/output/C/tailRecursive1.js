function factorial(n, acc) {
  while (true) {
    if ( ((n < 0) || (n === 0)) ) { return acc; }
    [n, acc] = [(n - 1), (acc * n)];
  }
}

