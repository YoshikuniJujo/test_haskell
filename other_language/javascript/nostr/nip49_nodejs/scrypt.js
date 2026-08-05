const crypto = require('node:crypto');

console.log(crypto.scryptSync("Hello", "salt", 10));
