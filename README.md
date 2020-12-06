Solutions to [Advent of Code 2020](https://adventofcode.com/2020)

For everything but day 1 the JS is in a comment in the elm

1. https://ellie-app.com/bHGvNkGWDcba1
2. https://ellie-app.com/bHCK59WZXd3a1
3. https://ellie-app.com/bJ4YTmnW9n4a1
4. https://ellie-app.com/bJycmLbmvGka1
5. https://ellie-app.com/bKhWrLkghYLa1
6.

```javascript
const groups = $0.innerText.split('\n\n');
const intersect = (l, r) => new Set(
    [...l].filter(v => r.has(v))
);
Array.prototype.sumSizes = function() {
    return this.reduce((s,v) => s + v.size, 0);
}
console.log({
    part1: groups.map(
            g => new Set(g.trim().split(/\s?/))
        ).sumSizes(),

    part2: groups.map(
            g => g.trim().split('\n').map(x => new Set(x)).reduce(intersect)
        ).sumSizes()
})
```
