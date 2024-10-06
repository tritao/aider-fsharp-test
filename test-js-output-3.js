function calculate(x, y) { return (x + y); }
class Options { constructor() { this.x = null; this.y = null; } }

const result = calculate(10, 20);
const options = new Options();
console.log(result, options.x, options.y); // Expected output: 30 null null
