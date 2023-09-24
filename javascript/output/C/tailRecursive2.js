function fibonacci(n, pprev, prev) {
  while (true) {
    if ( ((n < 0) || (n === 0)) ) { return pprev; }
    if ( (n === 1) ) { return prev; }
    [n, pprev, prev] = [(n - 1), prev, (pprev + prev)];
  }
}
