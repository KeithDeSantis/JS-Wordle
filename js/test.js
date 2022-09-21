const fs = require('fs');

const words = fs.readFileSync('../test.txt',
            {encoding:'utf8', flag:'r'});

console.log(words.split(/\r?\n/));