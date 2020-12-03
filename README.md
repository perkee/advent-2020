Solutions to [Advent of Code 2020](https://adventofcode.com/2020)

1. https://ellie-app.com/bHDKRHbgsQNa1 and the JS is lost to history
2. https://ellie-app.com/bHCK59WZXd3a1 also in JS, see comment in src/D02.elm
3. only in JS so far

```javascript
const rows = $0.innerText.trim().split('\n');
const trees3 = rows.reduce((sum,row,i) => sum + (row[(i * 3) % row.length] == '#'), 0);
console.log('part1', trees3);
let trees1 = 0, trees5 = 0, trees7 = 0, trees2 = 0;
const  getCol = (n, i) => ((i * n) % (rows[0].length));
for (let i = 0; i < rows.length; ++i) { 
  if (rows[i][getCol(1,i)] == '#') ++trees1;
  if (rows[i][getCol(5,i)] == '#') ++trees5;
  if (rows[i][getCol(7,i)] == '#') ++trees7;

};
for (let i = 0; i < rows.length; i+=2) {
  if (rows[i][(i/2) % (rows[0].length)] == '#') ++trees2;
};
console.log('part2', trees1 * trees2 * trees3 * trees5 * trees7);
```
