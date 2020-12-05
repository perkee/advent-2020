Solutions to [Advent of Code 2020](https://adventofcode.com/2020)

For everything but day 1 the JS is in a comment in the elm

1. https://ellie-app.com/bHGvNkGWDcba1 and the JS is lost to history
2. https://ellie-app.com/bHCK59WZXd3a1 also in JS, see comment in src/D02.elm
3. https://ellie-app.com/bJ4YTmnW9n4a1 see JS comment in src/D03.elm
4. https://ellie-app.com/bJycmLbmvGka1 JS
5.

    ```javascript
const seats = $0.innerText
  .trim()
  .split('\n')
  .map(
    s => parseInt(s.replace(/F|L/g, '0').replace(/B|R/g, '1'), 2)
  ).sort((a, b) => b - a);

console.log(
    'part1',
    seats[0] ,
    'part2',
    seats.find((v, i, a) => i && a[i - 1] - v > 1 ) + 1
);
```