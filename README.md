Solutions to [Advent of Code 2020](https://adventofcode.com/2020)

1. https://ellie-app.com/bHGvNkGWDcba1 and the JS is lost to history
2. https://ellie-app.com/bHCK59WZXd3a1 also in JS, see comment in src/D02.elm
3. only in JS so far

```javascript
const rows = $0.innerText.trim().split('\n');
const getCol = (n, i) =>
  ((i * n) % (rows[0].length));
const getCel = (v, h, i) =>
  (rows[i * v] || [])[ getCol(h, i) ] == '#';
const slopes = '11,13,15,17,21'
  .split(',').map(s => s.split(''));
const trees = slopes.map(([rise, run]) =>
  rows.reduce(
    (count, _, i) =>
      count + getCel(rise, run, i), 0
  )
);
console.log(
  'part1',
  trees[3],
  'part2',
  trees.reduce((p, n) => p * n)
);
```