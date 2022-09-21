// On page load.

const tileColors = {
    NotInWord: "#3D3D3D",
    WrongLocation: "#8B8000",
    RightLocation: "green",
    KeyNotInWord: "#222222"
};

document.addEventListener("DOMContentLoaded", () => {

    // Our word to be guessed
    const stringWord = allwords[Math.floor(Math.random() * allwords.length)];
    const word = stringWord.split("");

    // A dictionary tracking the number of time each letter occurs
    var letterDict = generateLetterCountDict(word);

    // Function to create the squares
    createSquares();

    // Array of arrays to create a matrix of guesses
    const guessedWords = [[]]
    // Counter to track where we are on the board
    let availableSpace = 1;

    const keys = document.querySelectorAll('.keyboard-row button');

    // Get the enter and delete buttons.
    const wideButtons = document.querySelectorAll('.keyboard-row button.wide-button');

    // Giving the buttons functionality
    for (let i = 0; i < keys.length; i++) {
        keys[i].onclick = ({ target }) => {
            const key = target.getAttribute("data-key");
            selectLetter(key);
            console.log(key);
        };
    }

    // Adding functionality to enter and delete.
    for (let j = 0; j < wideButtons.length; j++) {
        if (wideButtons[j].getAttribute("data-key") == "enter") {
            wideButtons[j].onclick = ({ target }) => {
                selectEnter();
                console.log("Enter pressed.");
            }
        } else {
            wideButtons[j].onclick = ({ target }) => {
                selectDelete();
                console.log("Delete pressed.");
            }
        }
    }

    //* Tracks the updated letters in the array
    function selectLetter(letter) {
        const currentWordArray = getCurrentWordArray();

        if (currentWordArray && currentWordArray.length < 5) {
            currentWordArray.push(letter);
            const availableSpaceElement = document.getElementById(String(availableSpace));
            availableSpace = availableSpace + 1;

            availableSpaceElement.textContent = letter;
        }
        else {
            console.log("Full row.");
            return;
        }
    }

    //* Submits the 5 letter word
    function selectEnter() {
        const currentWordArray = getCurrentWordArray();

        if (currentWordArray && currentWordArray.length == 5) {

            let result = evaluateGuess(getCurrentWordArray());
            guessedWords.push([]); // Move to the next line 

            // Check if a loss or win
            if (result) {
                window.alert("You won!");
                return;
            }
            else if (guessedWords.length > 6) {
                console.log("Game lost.");
                window.alert(`Game Over. The word is ${word.join('')}`);
                return 0;
            }
        }
        else {
            console.log("Not enough letters.");
        }
    }

    //* Deletes last letter entered
    function selectDelete() {
        const currentWordArray = getCurrentWordArray();
        if (currentWordArray.length > 0) {
            currentWordArray.pop();
            availableSpace = availableSpace - 1;
            const availableSpaceElement = document.getElementById(String(availableSpace));
            availableSpaceElement.textContent = "";
        }
    }

    //* Evaluates the guess and updates the board
    //* Return True if the guess was correct
    function evaluateGuess(guessArray) {
        console.log(guessArray);
        // Time interval between letters flipping
        let timeInterval = 300;

        // Generates "color array" map to map to the letters
        let colorArray = createColorArray(guessArray);

        // Apply each color in the map to the guess
        colorArray.forEach((color, index) => {

            setTimeout(() => {

                let evaledSquare = document.getElementById(String(availableSpace - 5 + index));
                evaledSquare.classList.add("animate__flipInX")

                evaledSquare.style.backgroundColor = color;

            }, timeInterval * index);
        });

        for (index in guessArray) {
            if (!word.includes(guessArray[index])) document.getElementById(guessArray[index]).style.backgroundColor = tileColors.KeyNotInWord;     
        };

        // Return true if the guess was correct
        return colorArray.every(element => element == tileColors.RightLocation);
    }

    //* Return an ordered array of colors that the squares will change to based on guess.
    //* __guessArray__ The player's guess
    function createColorArray(guessArray) {

        // Initialize the Color Array as all grey tiles.
        var colorArray = [tileColors.NotInWord, tileColors.NotInWord,
        tileColors.NotInWord, tileColors.NotInWord, tileColors.NotInWord,];

        // For each letter guessed we must assign a color
        guessArray.forEach((letter, index) => {

            // If the letter is correct, it will be green
            if (word[index] == guessArray[index]) colorArray[index] = tileColors.RightLocation;

            // If it is in the word but not in the right place, we mark it yellow
            else if (word.includes(guessArray[index])) colorArray[index] = tileColors.WrongLocation;

            // Helper function doubles back over the word every letter to ensure we only allow
            // as many copies of a letter as the word has, and that greens are prioritized.
            updatePriorColors(guessArray, colorArray, index);
        });
        
        // Return our final color array "map"
        return colorArray;
    }

    //* Retroactively update colors of letters after each letter is assigned its color
    function updatePriorColors(guessArray, colorArray, startingIndex) {

        // IF we are looking at the first letter there are no prior lettes to update
        if (startingIndex == 0) return;

        // Get the letter we are looking at
        letter = guessArray[startingIndex];
        // Get the number of times this letter occurs in the word
        numberOfLetterOccurences = letterDict[letter];

        // Helper function to determine if a square has an instance of our letter in the right place
        function correctLetter(index) { return colorArray[index] == tileColors.RightLocation && guessArray[index] == letter; }
        // Helper function to determine if a square has an instance of our letter in the wrong place
        function validYellow(index) { return !(colorArray[index] == tileColors.RightLocation) && guessArray[index] == letter; }

        //! To ensure we indicate the proper number of our letter in the word, 
        //! we must conform to numberOfLetterOccurences

        // First we count all the green tiles of our letter
        for (let i = startingIndex; i >= 0; i--) {
            // Decrement numberOfLetterOccurences for each we find
            if (correctLetter(i)) numberOfLetterOccurences -= 1;
        }

        // Then, we can start assigning yellows, up to our numberOfLetterOccurences left
        for (let j = startingIndex; j >= 0; j--) {
            // If the spot is a valid yellow...
            if (validYellow(j)) {
                // And there are still occurences to use...
                if (numberOfLetterOccurences > 0) {
                    // Assign it yellow and decrement the occurences
                    colorArray[j] = tileColors.WrongLocation;
                    numberOfLetterOccurences -= 1;
                }
                // And we've used all occurences, mark it grey
                else colorArray[j] = tileColors.NotInWord;
            }
        }
    }

    //* Create dictionary of each letter and the number of times they appear
    function generateLetterCountDict(wordArray) {
        var letterDict = {};

        wordArray.forEach((letter) => {
            if (letter in letterDict) {
                letterDict[letter] = letterDict[letter] + 1;
            } else {
                letterDict[letter] = 1;
            }
        });

        return letterDict;

    }

    //* Initializes the squares
    function createSquares() {
        const gameBoard = document.getElementById("board");
        for (let index = 0; index < 30; index++) {
            let square = document.createElement("div");
            square.classList.add("square");
            square.classList.add("animate__animated");
            square.setAttribute("id", index + 1);
            board.appendChild(square);
        }
    }

    //* Gets the current guess row
    function getCurrentWordArray() {
        const numberOfGuessedWords = guessedWords.length;
        return guessedWords[numberOfGuessedWords - 1];
    }

    document.addEventListener("keydown", function(event) {
        switch(event.keyCode) {
            case 13:
                selectEnter();
                break;
            case 8:
                selectDelete();
                break;
            default:
                try {
                    selectLetter(String.fromCharCode(event.keyCode).toLowerCase());
                } catch (error) {
                    console.log(error);
                }
                break;
    
        }
    });
});

altwords = ['which', 'there', 'their', 'about', 'would', 'these', 'other', 'words', 'could', 'write', 'first', 'water', 'after', 'where', 'right', 'think', 'three', 'years', 'place', 'sound', 'great', 'again', 'still', 'every', 'small', 'found', 'those', 'never', 'under', 'might', 'while', 'house', 
'world', 'below', 'asked', 'going', 'large', 'until', 'along', 'shall', 'being', 'often', 'earth', 'began', 'since', 'study', 'night', 'light', 'above', 'paper', 'parts', 'young', 'story', 'point', 'times', 'heard', 'whole', 'white', 'given', 'means', 'music', 'miles', 'thing', 'today', 'later', 'using', 'money', 'lines', 'order', 'group', 'among', 'learn', 'known', 'space', 'table', 'early', 'trees', 'short', 'hands', 'state', 'black', 'shown', 'stood', 'front', 'voice', 'kinds', 'makes', 'comes', 'close', 'power', 'lived', 'vowel', 'taken', 'built', 'heart', 'ready', 'quite', 'class', 'bring', 'round', 'horse', 'shows', 'piece', 'green', 'stand', 'birds', 'start', 'river', 'tried', 'least', 'field', 'whose', 'girls', 'leave', 'added', 'color', 'third', 'hours', 'moved', 'plant', 'doing', 'names', 'forms', 'heavy', 'ideas', 'cried', 'check', 'floor', 'begin', 'woman', 'alone', 'plane', 
'spell', 'watch', 'carry', 'wrote', 'clear', 'named', 'books', 'child', 'glass', 'human', 'takes', 'party', 'build', 'seems', 'blood', 'sides', 'seven', 'mouth', 'solve', 'north', 'value', 'death', 'maybe', 'happy', 'tells', 'gives', 'looks', 'shape', 'lives', 'steps', 'areas', 'sense', 'speak', 'force', 'ocean', 'speed', 'women', 'metal', 'south', 'grass', 'scale', 'cells', 'lower', 'sleep', 'wrong', 'pages', 'ships', 'needs', 'rocks', 'eight', 'major', 'level', 'total', 'ahead', 'reach', 'stars', 'store', 'sight', 'terms', 'catch', 'works', 'board', 'cover', 'songs', 'equal', 'stone', 'waves', 'guess', 'dance', 'spoke', 'break', 'cause', 'radio', 'weeks', 'lands', 'basic', 'liked', 'trade', 'fresh', 'final', 'fight', 'meant', 'drive', 'spent', 'local', 'waxes', 'knows', 'train', 'bread', 'homes', 'teeth', 'coast', 'thick', 'brown', 'clean', 'quiet', 'sugar', 'facts', 'steel', 'forth', 
'rules', 'notes', 'units', 'peace', 'month', 'verbs', 'seeds', 'helps', 'sharp', 'visit', 'woods', 'chief', 'walls', 'cross', 'wings', 'grown', 'cases', 'foods', 'crops', 'fruit', 'stick', 'wants', 'stage', 'sheep', 'nouns', 'plain', 'drink', 'bones', 'apart', 'turns', 'moves', 'touch', 'angle', 'based', 'range', 'marks', 'tired', 'older', 'farms', 'spend', 'shoes', 'goods', 'chair', 'twice', 'cents', 'empty', 'alike', 'style', 'broke', 'pairs', 'count', 'enjoy', 'score', 'shore', 'roots', 'paint', 'heads', 'shook', 'serve', 'angry', 'crowd', 'wheel', 'quick', 'dress', 'share', 'alive', 'noise', 'solid', 'cloth', 'signs', 'hills', 'types', 'drawn', 'worth', 'truck', 'piano', 'upper', 'loved', 'usual', 'faces', 'drove', 'cabin', 'boats', 'towns', 'proud', 'court', 'model', 'prime', 'fifty', 'plans', 'yards', 'prove', 'tools', 'price', 'sheet', 'smell', 'boxes', 'raise', 'match', 'truth', 
'roads', 'threw', 'enemy', 'lunch', 'chart', 'scene', 'graph', 'doubt', 'guide', 'winds', 'block', 'grain', 'smoke', 'mixed', 'games', 'wagon', 'sweet', 'topic', 'extra', 'plate', 'title', 'knife', 'fence', 'falls', 'cloud', 'wheat', 'plays', 'enter', 'broad', 'steam', 'atoms', 'press', 'lying', 'basis', 'clock', 'taste', 'grows', 'thank', 'storm', 'agree', 'brain', 'track', 'smile', 'funny', 'beach', 'stock', 'hurry', 'saved', 'sorry', 'giant', 'trail', 'offer', 'ought', 'rough', 'daily', 'avoid', 'keeps', 'throw', 'allow', 'cream', 'laugh', 'edges', 'teach', 'frame', 'bells', 'dream', 'magic', 'occur', 'ended', 'chord', 'false', 'skill', 'holes', 'dozen', 'brave', 'apple', 'climb', 'outer', 'pitch', 'ruler', 'holds', 'fixed', 'costs', 'calls', 'blank', 'staff', 'labor', 'eaten', 'youth', 'tones', 'honor', 'globe', 'gases', 'doors', 'poles', 'loose', 'apply', 'tears', 'exact', 'brush', 
'chest', 'layer', 'whale', 'minor', 'faith', 'tests', 'judge', 'items', 'worry', 'waste', 'hoped', 'strip', 'begun', 'aside', 'lakes', 'bound', 'depth', 'candy', 'event', 'worse', 'aware', 'shell', 'rooms', 'ranch', 'image', 'snake', 'aloud', 'dried', 'likes', 'motor', 'pound', 'knees', 'refer', 'fully', 'chain', 'shirt', 'flour', 'drops', 'spite', 'orbit', 'banks', 'shoot', 'curve', 'tribe', 'tight', 'blind', 'slept', 'shade', 'claim', 'flies', 'theme', 'queen', 'fifth', 'union', 'hence', 'straw', 'entry', 'issue', 'birth', 'feels', 'anger', 'brief', 'rhyme', 'glory', 'guard', 'flows', 'flesh', 'owned', 'trick', 'yours', 'sizes', 'noted', 'width', 'burst', 'route', 'lungs', 'uncle', 'bears', 'royal', 'kings', 'forty', 'trial', 'cards', 'brass', 'opera', 'chose', 'owner', 'vapor', 'beats', 'mouse', 'tough', 'wires', 'meter', 'tower', 'finds', 'inner', 'stuck', 'arrow', 'poems', 'label', 
'swing', 'solar', 'truly', 'tense', 'beans', 'split', 'rises', 'weigh', 'hotel', 'stems', 'pride', 'swung', 'grade', 'digit', 'badly', 'boots', 'pilot', 'sales', 'swept', 'lucky', 'prize', 'stove', 'tubes', 'acres', 'wound', 'steep', 'slide', 'trunk', 'error', 'porch', 'slave', 'exist', 'faced', 'mines', 'marry', 'juice', 'raced', 'waved', 'goose', 'trust', 'fewer', 'favor', 'mills', 'views', 'joint', 'eager', 'spots', 'blend', 'rings', 'adult', 'index', 'nails', 'horns', 'balls', 'flame', 'rates', 'drill', 'trace', 'skins', 'waxed', 'seats', 'stuff', 'ratio', 'minds', 'dirty', 'silly', 'coins', 'hello', 'trips', 'leads', 'rifle', 'hopes', 'bases', 'shine', 'bench', 'moral', 'fires', 'meals', 'shake', 'shops', 'cycle', 'movie', 'slope', 'canoe', 'teams', 'folks', 'fired', 'bands', 'thumb', 'shout', 'canal', 'habit', 'reply', 'ruled', 'fever', 'crust', 'shelf', 'walks', 'midst', 'crack', 
'print', 'tales', 'coach', 'stiff', 'flood', 'verse', 'awake', 'rocky', 'march', 'fault', 'swift', 'faint', 'civil', 'ghost', 'feast', 'blade', 'limit', 'germs', 'reads', 'ducks', 'dairy', 'worst', 'gifts', 'lists', 'stops', 'rapid', 'brick', 'claws', 'beads', 'beast', 'skirt', 'cakes', 'lions', 'frogs', 'tries', 'nerve', 'grand', 'armed', 'treat', 'honey', 'moist', 'legal', 'penny', 'crown', 'shock', 'taxes', 'sixty', 'altar', 'pulls', 'sport', 'drums', 'talks', 'dying', 'dates', 'drank', 'blows', 'lever', 'wages', 'proof', 'drugs', 'tanks', 'sings', 'tails', 'pause', 'herds', 'arose', 'hated', 'clues', 'novel', 'shame', 'burnt', 'races', 'flash', 'weary', 'heels', 'token', 'coats', 'spare', 'shiny', 'alarm', 'dimes', 'sixth', 'clerk', 'mercy', 'sunny', 'guest', 'float', 'shone', 'pipes', 'worms', 'bills', 'sweat', 'suits', 'smart', 'upset', 'rains', 'sandy', 'rainy', 'parks', 'sadly', 
'fancy', 'rider', 'unity', 'bunch', 'rolls', 'crash', 'craft', 'newly', 'gates', 'hatch', 'paths', 'funds', 'wider', 'grace', 'grave', 'tides', 'admit', 'shift', 'sails', 'pupil', 'tiger', 'angel', 'cruel', 'agent', 'drama', 'urged', 'patch', 'nests', 'vital', 'sword', 'blame', 'weeds', 'screw', 'vocal', 'bacon', 'chalk', 'cargo', 'crazy', 'acted', 'goats', 'arise', 'witch', 'loves', 'queer', 'dwell', 'backs', 'ropes', 'shots', 'merry', 'phone', 'cheek', 'peaks', 'ideal', 'beard', 'eagle', 'creek', 'cries', 'ashes', 'stall', 'yield', 'mayor', 'opens', 'input', 'fleet', 'tooth', 'cubic', 'wives', 'burns', 'poets', 'apron', 'spear', 'organ', 'cliff', 'stamp', 'paste', 'rural', 'baked', 'chase', 'slice', 'slant', 'knock', 'noisy', 'sorts', 'stays', 'wiped', 'blown', 'piled', 'clubs', 'cheer', 'widow', 'twist', 'tenth', 'hides', 'comma', 'sweep', 'spoon', 'stern', 'crept', 'maple', 'deeds', 
'rides', 'muddy', 'crime', 'jelly', 'ridge', 'drift', 'dusty', 'devil', 'tempo', 'humor', 'sends', 'steal', 'tents', 'waist', 'roses', 'reign', 'noble', 'cheap', 'dense', 'linen', 'geese', 'woven', 'posts', 'hired', 'wrath', 'salad', 'bowed', 'tires', 'shark', 'belts', 'grasp', 'blast', 'polar', 'fungi', 'tends', 'pearl', 'loads', 'jokes', 'veins', 'frost', 'hears', 'loses', 'hosts', 'diver', 'phase', 'toads', 'alert', 'tasks', 'seams', 'coral', 'focus', 'naked', 'puppy', 'jumps', 'spoil', 'quart', 'macro', 'fears', 'flung', 'spark', 'vivid', 'brook', 'steer', 'spray', 'decay', 'ports', 'socks', 'urban', 'goals', 'grant', 'minus', 'films', 'tunes', 'shaft', 'firms', 'skies', 'bride', 'wreck', 'flock', 'stare', 'hobby', 'bonds', 'dared', 'faded', 'thief', 'crude', 'pants', 'flute', 'votes', 'tonal', 'radar', 'wells', 'skull', 'hairs', 'argue', 'wears', 'dolls', 'voted', 'caves', 'cared', 
'broom', 'scent', 'panel', 'fairy', 'olive', 'bends', 'prism', 'lamps', 'cable', 'peach', 'ruins', 'rally', 'schwa', 'lambs', 'sells', 'cools', 'draft', 'charm', 'limbs', 'brake', 'gazed', 'cubes', 'delay', 'beams', 'fetch', 'ranks', 'array', 'harsh', 'camel', 'vines', 'picks', 'naval', 'purse', 'rigid', 'crawl', 'toast', 'soils', 'sauce', 'basin', 'ponds', 'twins', 'wrist', 'fluid', 'pools', 'brand', 'stalk', 'robot', 'reeds', 'hoofs', 'buses', 'sheer', 'grief', 'bloom', 'dwelt', 'melts', 'risen', 'flags', 'knelt', 'fiber', 'roofs', 'freed', 'armor', 'piles', 'aimed', 'algae', 'twigs', 'lemon', 'ditch', 'drunk', 'rests', 'chill', 'slain', 'panic', 'cords', 'tuned', 'crisp', 'ledge', 'dived', 'swamp', 'clung', 'stole', 'molds', 'yarns', 'liver', 'gauge', 'breed', 'stool', 'gulls', 'awoke', 'gross', 'diary', 'rails', 'belly', 'trend', 'flask', 'stake', 'fried', 'draws', 'actor', 'handy', 
'bowls', 'haste', 'scope', 'deals', 'knots', 'moons', 'essay', 'thump', 'hangs', 'bliss', 'dealt', 'gains', 'bombs', 'clown', 'palms', 'cones', 'roast', 'tidal', 'bored', 'chant', 'acids', 'dough', 'camps', 'swore', 'lover', 'hooks', 'males', 'cocoa', 'punch', 'award', 'reins', 'ninth', 'noses', 'links', 'drain', 'fills', 'nylon', 'lunar', 'pulse', 'flown', 'elbow', 'fatal', 'sites', 'moths', 'meats', 'foxes', 'mined', 'attic', 'fiery', 'mount', 'usage', 'swear', 'snowy', 'rusty', 'scare', 'traps', 'relax', 'react', 'valid', 'robin', 'cease', 'gills', 'prior', 'safer', 'polio', 'loyal', 'swell', 'salty', 'marsh', 'vague', 'weave', 'mound', 'seals', 'mules', 'virus', 'scout', 'acute', 'windy', 'stout', 'folds', 'seize', 'hilly', 'joins', 'pluck', 'stack', 'lords', 'dunes', 'burro', 'hawks', 'trout', 'feeds', 'scarf', 'halls', 'coals', 'towel', 'souls', 'elect', 'buggy', 'pumps', 'loans', 
'spins', 'files', 'oxide', 'pains', 'photo', 'rival', 'flats', 'syrup', 'rodeo', 'sands', 'moose', 'pints', 'curly', 'comic', 'cloak', 'onion', 'clams', 'scrap', 'didst', 'couch', 'codes', 'fails', 'ounce', 'lodge', 'greet', 'gypsy', 'utter', 'paved', 'zones', 'fours', 'alley', 'tiles', 'bless', 'crest', 'elder', 'kills', 'yeast', 'erect', 'bugle', 'medal', 'roles', 'hound', 'snail', 'alter', 'ankle', 'relay', 'loops', 'zeros', 'bites', 'modes', 'debts', 'realm', 'glove', 'rayon', 'swims', 'poked', 'stray', 'lifts', 'maker', 'lumps', 'graze', 'dread', 'barns', 'docks', 'masts', 'pours', 'wharf', 'curse', 'plump', 'robes', 'seeks', 'cedar', 'curls', 'jolly', 'myths', 'cages', 'gloom', 'locks', 'pedal', 'beets', 'crows', 'anode', 'slash', 'creep', 'rowed', 'chips', 'fists', 'wines', 'cares', 'valve', 'newer', 'motel', 'ivory', 'necks', 'clamp', 'barge', 'blues', 'alien', 'frown', 'strap', 
'crews', 'shack', 'gonna', 'saves', 'stump', 'ferry', 'idols', 'cooks', 'juicy', 'glare', 'carts', 'alloy', 'bulbs', 'lawns', 'lasts', 'fuels', 'oddly', 'crane', 'filed', 'weird', 'shawl', 'slips', 'troop', 'bolts', 'suite', 'sleek', 'quilt', 'tramp', 'blaze', 'atlas', 'odors', 'scrub', 'crabs', 'probe', 'logic', 'adobe', 'exile', 'rebel', 'grind', 'sting', 'spine', 'cling', 'desks', 'grove', 'leaps', 'prose', 'lofty', 'agony', 'snare', 'tusks', 'bulls', 'moods', 'humid', 'finer', 'dimly', 'plank', 'china', 'pines', 'guilt', 'sacks', 'brace', 'quote', 'lathe', 'gaily', 'fonts', 'scalp', 'adopt', 'foggy', 'ferns', 'grams', 'clump', 'perch', 'tumor', 'teens', 'crank', 'fable', 'hedge', 'genes', 'sober', 'boast', 'tract', 'cigar', 'unite', 'owing', 'thigh', 'haiku', 'swish', 'dikes', 'wedge', 'booth', 'eased', 'frail', 'cough', 'tombs', 'darts', 'forts', 'choir', 'pouch', 'pinch', 'hairy', 
'buyer', 'torch', 'vigor', 'waltz', 'heats', 'herbs', 'users', 'flint', 'click', 'madam', 'bleak', 'blunt', 'aided', 'lacks', 'masks', 'waded', 'risks', 'nurse', 'chaos', 'sewed', 'cured', 'ample', 'lease', 'steak', 'sinks', 'merit', 'bluff', 'bathe', 'gleam', 'bonus', 'colts', 'shear', 'gland', 'silky', 'skate', 'birch', 'anvil', 'sleds', 'groan', 'maids', 'meets', 'speck', 'hymns', 'hints', 'drown', 'bosom', 'slick', 'quest', 'coils', 'spied', 'snows', 'stead', 'snack', 'plows', 'blond', 'tamed', 'thorn', 'waits', 'glued', 'banjo', 'tease', 'arena', 'bulky', 'carve', 'stunt', 'warms', 'shady', 'razor', 'folly', 'leafy', 'notch', 'fools', 'otter', 'pears', 'flush', 'genus', 'ached', 'fives', 'flaps', 'spout', 'smote', 'fumes', 'adapt', 'cuffs', 'tasty', 'stoop', 'clips', 'disks', 'sniff', 'lanes', 'brisk', 'imply', 'demon', 'super', 'furry', 'raged', 'growl', 'texts', 'hardy', 'stung', 
'typed', 'hates', 'wiser', 'timid', 'serum', 'beaks', 'rotor', 'casts', 'baths', 'glide', 'plots', 'trait', 'resin', 'slums', 'lyric', 'puffs', 'decks', 'brood', 'mourn', 'aloft', 'abuse', 'whirl', 'edged', 'ovary', 'quack', 'heaps', 'slang', 'await', 'civic', 'saint', 'bevel', 'sonar', 'aunts', 'packs', 'froze', 'tonic', 'corps', 'swarm', 'frank', 'repay', 'gaunt', 'wired', 'niece', 'cello', 'needy', 'chuck', 'stony', 'media', 'surge', 'hurts', 'repel', 'husky', 'dated', 'hunts', 'mists', 'exert', 'dries', 'mates', 'sworn', 'baker', 'spice', 'oasis', 'boils', 'spurs', 'doves', 'sneak', 'paces', 'colon', 'siege', 'strum', 'drier', 'cacao', 'humus', 'bales', 'piped', 'nasty', 'rinse', 'boxer', 'shrub', 'amuse', 'tacks', 'cited', 'slung', 'delta', 'laden', 'larva', 'rents', 'yells', 'spool', 'spill', 'crush', 'jewel', 'snaps', 'stain', 'kicks', 'tying', 'slits', 'rated', 'eerie', 'smash', 
'plums', 'zebra', 'earns', 'bushy', 'scary', 'squad', 'tutor', 'silks', 'slabs', 'bumps', 'evils', 'fangs', 'snout', 'peril', 'pivot', 'yacht', 'lobby', 'jeans', 'grins', 'viola', 'liner', 'comet', 'scars', 'chops', 'raids', 'eater', 'slate', 'skips', 'soles', 'misty', 'urine', 'knobs', 'sleet', 'holly', 'pests', 'forks', 'grill', 'trays', 'pails', 'borne', 'tenor', 'wares', 'carol', 'woody', 'canon', 'wakes', 'kitty', 'miner', 'polls', 'shaky', 'nasal', 'scorn', 'chess', 'taxis', 'crate', 'shyly', 'tulip', 'forge', 'nymph', 'budge', 'lowly', 'abide', 'depot', 'oases', 'asses', 'sheds', 'fudge', 'pills', 'rivet', 'thine', 'groom', 'lanky', 'boost', 'broth', 'heave', 'gravy', 'beech', 'timed', 'quail', 'inert', 'gears', 'chick', 'hinge', 'trash', 'clash', 'sighs', 'renew', 'bough', 'dwarf', 'slows', 'quill', 'shave', 'spore', 'sixes', 'chunk', 'madly', 'paced', 'braid', 'fuzzy', 'motto', 
'spies', 'slack', 'mucus', 'magma', 'awful', 'discs', 'erase', 'posed', 'asset', 'cider', 'taper', 'theft', 'churn', 'satin', 'slots', 'taxed', 'bully', 'sloth', 'shale', 'tread', 'raked', 'curds', 'manor', 'aisle', 'bulge', 'loins', 'stair', 'tapes', 'leans', 'bunks', 'squat', 'towed', 'lance', 'panes', 'sakes', 'heirs', 'caste', 'dummy', 'pores', 'fauna', 'crook', 'poise', 'epoch', 'risky', 'warns', 'fling', 'berry', 'grape', 'flank', 'drags', 'squid', 'pelts', 'icing', 'irony', 'irons', 'barks', 'whoop', 'choke', 'diets', 'whips', 'tally', 'dozed', 'twine', 'kites', 'bikes', 'ticks', 'riots', 'roars', 'vault', 'looms', 'scold', 'blink', 'dandy', 'pupae', 'sieve', 'spike', 'ducts', 'lends', 'pizza', 'brink', 'widen', 'plumb', 'pagan', 'feats', 'bison', 'soggy', 'scoop', 'argon', 'nudge', 'skiff', 'amber', 'sexes', 'rouse', 'salts', 'hitch', 'exalt', 'leash', 'dined', 'chute', 'snort', 
'gusts', 'melon', 'cheat', 'reefs', 'llama', 'lasso', 'debut', 'quota', 'oaths', 'prone', 'mixes', 'rafts', 'dives', 'stale', 'inlet', 'flick', 'pinto', 'brows', 'untie', 'batch', 'greed', 'chore', 'stirs', 'blush', 'onset', 'barbs', 'volts', 'beige', 'swoop', 'paddy', 'laced', 'shove', 'jerky', 'poppy', 'leaks', 'fares', 'dodge', 'godly', 'squaw', 'affix', 'brute', 'nicer', 'undue', 'snarl', 'merge', 'doses', 'showy', 'daddy', 'roost', 'vases', 'swirl', 'petty', 'colds', 'curry', 'cobra', 'genie', 'flare', 'messy', 'cores', 'soaks', 'ripen', 'whine', 'amino', 'plaid', 'spiny', 'mowed', 'baton', 'peers', 'vowed', 'pious', 'swans', 'exits', 'afoot', 'plugs', 'idiom', 'chili', 'rites', 'serfs', 'cleft', 'berth', 'grubs', 'annex', 'dizzy', 'hasty', 'latch', 'wasps', 'mirth', 'baron', 'plead', 'aloof', 'aging', 'pixel', 'bared', 'mummy', 'hotly', 'auger', 'buddy', 'chaps', 'badge', 'stark', 
'fairs', 'gully', 'mumps', 'emery', 'filly', 'ovens', 'drone', 'gauze', 'idiot', 'fussy', 'annoy', 'shank', 'gouge', 'bleed', 'elves', 'roped', 'unfit', 'baggy', 'mower', 'scant', 'grabs', 'fleas', 'lousy', 'album', 'sawed', 'cooky', 'murky', 'infer', 'burly', 'waged', 'dingy', 'brine', 'kneel', 'creak', 'vanes', 'smoky', 'spurt', 'combs', 'easel', 'laces', 'humps', 'rumor', 'aroma', 'horde', 'swiss', 'leapt', 'opium', 'slime', 'afire', 'pansy', 'mares', 'soaps', 'husks', 'snips', 'hazel', 'lined', 'cafes', 'naive', 'wraps', 'sized', 'piers', 'beset', 'agile', 'tongs', 'steed', 'fraud', 'booty', 'valor', 'downy', 'witty', 'mossy', 'psalm', 'scuba', 'tours', 'polka', 'milky', 'gaudy', 'shrug', 'tufts', 'wilds', 'laser', 'truss', 'hares', 'creed', 'lilac', 'siren', 'tarry', 'bribe', 'swine', 'muted', 'flips', 'cures', 'sinew', 'boxed', 'hoops', 'gasps', 'hoods', 'niche', 'yucca', 'glows', 
'sewer', 'whack', 'fuses', 'gowns', 'droop', 'bucks', 'pangs', 'mails', 'whisk', 'haven', 'clasp', 'sling', 'stint', 'urges', 'champ', 'piety', 'chirp', 'pleat', 'posse', 'sunup', 'menus', 'howls', 'quake', 'knack', 'plaza', 'fiend', 'caked', 'bangs', 'erupt', 'poker', 'olden', 'cramp', 'voter', 'poses', 'manly', 'slump', 'fined', 'grips', 'gaped', 'purge', 'hiked', 'maize', 'fluff', 'strut', 'sloop', 'prowl', 'roach', 'cocks', 'bland', 'dials', 'plume', 'slaps', 'soups', 'dully', 'wills', 'foams', 'solos', 'skier', 'eaves', 'totem', 'fused', 'latex', 'veils', 'mused', 'mains', 'myrrh', 'racks', 'galls', 'gnats', 'bouts', 'sisal', 'shuts', 'hoses', 'dryly', 'hover', 'gloss', 'seeps', 'denim', 'putty', 'guppy', 'leaky', 'dusky', 'filth', 'oboes', 'spans', 'fowls', 'adorn', 'glaze', 'haunt', 'dares', 'obeys', 'bakes', 'abyss', 'smelt', 'gangs', 'aches', 'trawl', 'claps', 'undid', 'spicy', 
'hoist', 'fades', 'vicar', 'acorn', 'pussy', 'gruff', 'musty', 'tarts', 'snuff', 'hunch', 'truce', 'tweed', 'dryer', 'loser', 'sheaf', 'moles', 'lapse', 'tawny', 'vexed', 'autos', 'wager', 'domes', 'sheen', 'clang', 'spade', 'sowed', 'broil', 'slyly', 'studs', 'grunt', 'donor', 'slugs', 'aspen', 'homer', 'croak', 'tithe', 'halts', 'avert', 'havoc', 'hogan', 'glint', 'ruddy', 'jeeps', 'flaky', 'ladle', 'taunt', 'snore', 'fines', 'props', 'prune', 'pesos', 'radii', 'pokes', 'tiled', 'daisy', 'heron', 'villa', 'farce', 'binds', 'cites', 'fixes', 'jerks', 'livid', 'waked', 'inked', 'booms', 'chews', 'licks', 'hyena', 'scoff', 'lusty', 'sonic', 'smith', 'usher', 'tucks', 'vigil', 'molts', 'sects', 'spars', 'dumps', 'scaly', 'wisps', 'sores', 'mince', 'panda', 'flier', 'axles', 'plied', 'booby', 'patio', 'rabbi', 'petal', 'polyp', 'tints', 'grate', 'troll', 'tolls', 'relic', 'phony', 'bleat', 
'flaws', 'flake', 'snags', 'aptly', 'drawl', 'ulcer', 'soapy', 'bossy', 'monks', 'crags', 'caged', 'twang', 'diner', 'taped', 'cadet', 'grids', 'spawn', 'guile', 'noose', 'mores', 'girth', 'slimy', 'aides', 'spasm', 'burrs', 'alibi', 'lymph', 'saucy', 'muggy', 'liter', 'joked', 'goofy', 'exams', 'enact', 'stork', 'lured', 'toxic', 'omens', 'nears', 'covet', 'wrung', 'forum', 'venom', 'moody', 'alder', 'sassy', 'flair', 'guild', 'prays', 'wrens', 'hauls', 'stave', 'tilts', 'pecks', 'stomp', 'gales', 'tempt', 'capes', 'mesas', 'omits', 'tepee', 'harry', 'wring', 'evoke', 'limes', 'cluck', 'lunge', 'highs', 'canes', 'giddy', 'lithe', 'verge', 'khaki', 'queue', 'loath', 'foyer', 'outdo', 'fared', 'deter', 'crumb', 'astir', 'spire', 'jumpy', 'extol', 'buoys', 'stubs', 'lucid', 'thong', 'afore', 'whiff', 'maxim', 'hulls', 'clogs', 'slats', 'jiffy', 'arbor', 'cinch', 'igloo', 'goody', 'gazes', 
'dowel', 'calms', 'bitch', 'scowl', 'gulps', 'coded', 'waver', 'mason', 'lobes', 'ebony', 'flail', 'isles', 'clods', 'dazed', 'adept', 'oozed', 'sedan', 'clays', 'warts', 'ketch', 'skunk', 'manes', 'adore', 'sneer', 'mango', 'fiord', 'flora', 'roomy', 'minks', 'thaws', 'watts', 'freer', 'exult', 'plush', 'paled', 'twain', 'clink', 'scamp', 'pawed', 'grope', 'bravo', 'gable', 'stink', 'sever', 'waned', 'rarer', 'regal', 'wards', 'fawns', 'babes', 'unify', 'amend', 'oaken', 'glade', 'visor', 'hefty', 'nines', 'throb', 'pecan', 'butts', 'pence', 'sills', 'jails', 'flyer', 'saber', 'nomad', 'miter', 'beeps', 'domed', 'gulfs', 'curbs', 'heath', 'moors', 'aorta', 'larks', 'tangy', 'wryly', 'cheep', 'rages', 'evade', 'lures', 'freak', 'vogue', 'tunic', 'slams', 'knits', 'dumpy', 'mania', 'spits', 'firth', 'hikes', 'trots', 'nosed', 'clank', 'dogma', 'bloat', 'balsa', 'graft', 'middy', 'stile', 
'keyed', 'finch', 'sperm', 'chaff', 'wiles', 'amigo', 'copra', 'amiss', 'eying', 'twirl', 'lurch', 'popes', 'chins', 'smock', 'tines', 'guise', 'grits', 'junks', 'shoal', 'cache', 'tapir', 'atoll', 'deity', 'toils', 'spree', 'mocks', 'scans', 'shorn', 'revel', 'raven', 'hoary', 'reels', 'scuff', 'mimic', 'weedy', 'corny', 'truer', 'rouge', 'ember', 'floes', 'torso', 'wipes', 'edict', 'sulky', 'recur', 'groin', 'baste', 'kinks', 'surer', 'piggy', 'moldy', 'franc', 'liars', 'inept', 'gusty', 'facet', 'jetty', 'equip', 'leper', 'slink', 'soars', 'cater', 'dowry', 'sided', 'yearn', 'decoy', 'taboo', 'ovals', 'heals', 'pleas', 'beret', 'spilt', 'gayly', 'rover', 'endow', 'pygmy', 'carat', 'abbey', 'vents', 'waken', 'chimp', 'fumed', 'sodas', 'vinyl', 'clout', 'wades', 'mites', 'smirk', 'bores', 'bunny', 'surly', 'frock', 'foray', 'purer', 'milks', 'query', 'mired', 'blare', 'froth', 'gruel', 
'navel', 'paler', 'puffy', 'casks', 'grime', 'derby', 'mamma', 'gavel', 'teddy', 'vomit', 'moans', 'allot', 'defer', 'wield', 'viper', 'louse', 'erred', 'hewed', 'abhor', 'wrest', 'waxen', 'adage', 'ardor', 'stabs', 'pored', 'rondo', 'loped', 'fishy', 'bible', 'hires', 'foals', 'feuds', 'jambs', 'thuds', 'jeers', 'knead', 'quirk', 'rugby', 'expel', 'greys', 'rigor', 'ester', 'lyres', 'aback', 'glues', 'lotus', 'lurid', 'rungs', 'hutch', 'thyme', 'valet', 'tommy', 'yokes', 'epics', 'trill', 'pikes', 'ozone', 'caper', 'chime', 'frees', 'famed', 'leech', 'smite', 'neigh', 'erode', 'robed', 'hoard', 'salve', 'conic', 'gawky', 'craze', 'jacks', 'gloat', 'mushy', 'rumps', 'fetus', 'wince', 'pinks', 'shalt', 'toots', 'glens', 'cooed', 'rusts', 'stews', 'shred', 'parka', 'chugs', 'winks', 'clots', 'shrew', 'booed', 'filmy', 'juror', 'dents', 'gummy', 'grays', 'hooky', 'butte', 'dogie', 'poled', 
'reams', 'fifes', 'spank', 'gayer', 'tepid', 'spook', 'taint', 'flirt', 'rogue', 'spiky', 'opals', 'miser', 'cocky', 'coyly', 'balmy', 'slosh', 'brawl', 'aphid', 'faked', 'hydra', 'brags', 'chide', 'yanks', 'allay', 'video', 'altos', 'eases', 'meted', 'chasm', 'longs', 'excel', 'taffy', 'impel', 'savor', 'koala', 'quays', 'dawns', 'proxy', 'clove', 'duets', 'dregs', 'tardy', 'briar', 'grimy', 'ultra', 'meaty', 'halve', 'wails', 'suede', 'mauve', 'envoy', 'arson', 'coves', 'gooey', 'brews', 'sofas', 'chums', 'amaze', 'zooms', 'abbot', 'halos', 'scour', 'suing', 'cribs', 'sagas', 'enema', 'wordy', 'harps', 'coupe', 'molar', 'flops', 'weeps', 'mints', 'ashen', 'felts', 'askew', 'munch', 'mewed', 'divan', 'vices', 'jumbo', 'blobs', 'blots', 'spunk', 'acrid', 'topaz', 'cubed', 'clans', 'flees', 'slurs', 'gnaws', 'welds', 'fords', 'emits', 'agate', 'pumas', 'mends', 'darks', 'dukes', 'plies', 
'canny', 'hoots', 'oozes', 'lamed', 'fouls', 'clefs', 'nicks', 'mated', 'skims', 'brunt', 'tuber', 'tinge', 'fates', 'ditty', 'thins', 'frets', 'eider', 'bayou', 'mulch', 'fasts', 'amass', 'damps', 'morns', 'friar', 'palsy', 'vista', 'croon', 'conch', 'udder', 'tacos', 'skits', 'mikes', 'quits', 'preen', 'aster', 'adder', 'elegy', 'pulpy', 'scows', 'baled', 'hovel', 'lavas', 'crave', 'optic', 'welts', 'busts', 'knave', 'razed', 'shins', 'totes', 'scoot', 'dears', 'crock', 'mutes', 'trims', 'skein', 'doted', 'shuns', 'veers', 'fakes', 'yoked', 'wooed', 'hacks', 'sprig', 'wands', 'lulls', 'seers', 'snobs', 'nooks', 'pined', 'perky', 'mooed', 'frill', 'dines', 'booze', 'tripe', 'prong', 'drips', 'odder', 'levee', 'antic', 'sidle', 'pithy', 'corks', 'yelps', 'joker', 'fleck', 'buffs', 'scram', 'tiers', 'bogey', 'doled', 'irate', 'vales', 'coped', 'hails', 'elude', 'bulks', 'aired', 'vying', 
'stags', 'strew', 'cocci', 'pacts', 'scabs', 'silos', 'dusts', 'yodel', 'terse', 'jaded', 'baser', 'jibes', 'foils', 'sways', 'forgo', 'slays', 'preys', 'treks', 'quell', 'peeks', 'assay', 'lurks', 'eject', 'boars', 'trite', 'belch', 'gnash', 'wanes', 'lutes', 'whims', 'dosed', 'chewy', 'snipe', 'umbra', 'teems', 'dozes', 'kelps', 'upped', 'brawn', 'doped', 'shush', 'rinds', 'slush', 'moron', 'voile', 'woken', 'fjord', 'sheik', 'jests', 'kayak', 'slews', 'toted', 'saner', 'drape', 'patty', 'raves', 'sulfa', 'grist', 'skied', 'vixen', 'civet', 'vouch', 'tiara', 'homey', 'moped', 'runts', 'serge', 'kinky', 'rills', 'corns', 'brats', 'pries', 'amble', 'fries', 'loons', 'tsars', 'datum', 'musky', 'pigmy', 'gnome', 'ravel', 'ovule', 'icily', 'liken', 'lemur', 'frays', 'silts', 'sifts', 'plods', 'ramps', 'tress', 'earls', 'dudes', 'waive', 'karat', 'jolts', 'peons', 'beers', 'horny', 'pales', 
'wreak', 'lairs', 'lynch', 'stank', 'swoon', 'idler', 'abort', 'blitz', 'ensue', 'atone', 'bingo', 'roves', 'kilts', 'scald', 'adios', 'cynic', 'dulls', 'memos', 'elfin', 'dales', 'peels', 'peals', 'bares', 'sinus', 'crone', 'sable', 'hinds', 'shirk', 'enrol', 'wilts', 'roams', 'duped', 'cysts', 'mitts', 'safes', 'spats', 'coops', 'filet', 'knell', 'refit', 'covey', 'punks', 'kilns', 'fitly', 'abate', 'talcs', 'heeds', 'duels', 'wanly', 'ruffs', 'gauss', 'lapel', 'jaunt', 'whelp', 'cleat', 'gauzy', 'dirge', 'edits', 'wormy', 'moats', 'smear', 'prods', 'bowel', 'frisk', 'vests', 'bayed', 'rasps', 'tames', 'delve', 'embed', 'befit', 'wafer', 'ceded', 'novas', 'feign', 'spews', 'larch', 'huffs', 'doles', 'mamas', 'hulks', 'pried', 'brims', 'irked', 'aspic', 'swipe', 'mealy', 'skimp', 'bluer', 'slake', 'dowdy', 'penis', 'brays', 'pupas', 'egret', 'flunk', 'phlox', 'gripe', 'peony', 'douse', 
'blurs', 'darns', 'slunk', 'lefts', 'chats', 'inane', 'vials', 'stilt', 'rinks', 'woofs', 'wowed', 'bongs', 'frond', 'ingot', 'evict', 'singe', 'shyer', 'flied', 'slops', 'dolts', 'drool', 'dells', 'whelk', 'hippy', 'feted', 'ether', 'cocos', 'hives', 'jibed', 'mazes', 'trios', 'sirup', 'squab', 'laths', 'leers', 'pasta', 'rifts', 'lopes', 'alias', 'whirs', 'diced', 'slags', 'lodes', 'foxed', 'idled', 'prows', 'plait', 'malts', 'chafe', 'cower', 'toyed', 'chefs', 'keels', 'sties', 'racer', 'etude', 'sucks', 'sulks', 'micas', 'czars', 'copse', 'ailed', 'abler', 'rabid', 'golds', 'croup', 'snaky', 'visas', 'palls', 'mopes', 'boned', 'wispy', 'raved', 'swaps', 'junky', 'doily', 'pawns', 'tamer', 'poach', 'baits', 'damns', 'gumbo', 'daunt', 'prank', 'hunks', 'buxom', 'heres', 'honks', 'stows', 'unbar', 'idles', 'routs', 'sages', 'goads', 'remit', 'copes', 'deign', 'culls', 'girds', 'haves', 
'lucks', 'stunk', 'dodos', 'shams', 'snubs', 'icons', 'usurp', 'dooms', 'hells', 'soled', 'comas', 'paves', 'maths', 'perks', 'limps', 'wombs', 'blurb', 'daubs', 'cokes', 'sours', 'stuns', 'cased', 'musts', 'coeds', 'cowed', 'aping', 'zoned', 'rummy', 'fetes', 'skulk', 'quaff', 'rajah', 'deans', 'reaps', 'galas', 'tills', 'roved', 'kudos', 'toned', 'pared', 'scull', 'vexes', 'punts', 'snoop', 'bails', 'dames', 'hazes', 'lores', 'marts', 'voids', 'ameba', 'rakes', 'adzes', 'harms', 'rears', 'satyr', 'swill', 'hexes', 'colic', 'leeks', 'hurls', 'yowls', 'ivies', 'plops', 'musks', 'papaw', 'jells', 'bused', 'cruet', 'bided', 'filch', 'zests', 'rooks', 'laxly', 'rends', 'loams', 'basks', 'sires', 'carps', 'pokey', 'flits', 'muses', 'bawls', 'shuck', 'viler', 'lisps', 'peeps', 'sorer', 'lolls', 'prude', 'diked', 'floss', 'flogs', 'scums', 'dopes', 'bogie', 'pinky', 'leafs', 'tubas', 'scads', 
'lowed', 'yeses', 'biked', 'qualm', 'evens', 'caned', 'gawks', 'whits', 'wooly', 'gluts', 'romps', 'bests', 'dunce', 'crony', 'joist', 'tunas', 'boner', 'malls', 'parch', 'avers', 'crams', 'pares', 'dally', 'bigot', 'kales', 'flays', 'leach', 'gushy', 'pooch', 'huger', 'slyer', 'golfs', 'mires', 'flues', 'loafs', 'arced', 'acnes', 'neons', 'fiefs', 'dints', 'dazes', 'pouts', 'cored', 'yules', 'lilts', 'beefs', 'mutts', 'fells', 'cowls', 'spuds', 'lames', 'jawed', 'dupes', 'deads', 'bylaw', 'noons', 'nifty', 'clued', 'vireo', 'gapes', 'metes', 'cuter', 'maims', 'droll', 'cupid', 'mauls', 'sedge', 'papas', 'wheys', 'eking', 'loots', 'hilts', 'meows', 'beaus', 'dices', 'peppy', 'riper', 'fogey', 'gists', 'yogas', 'gilts', 'skews', 'cedes', 'zeals', 'alums', 'okays', 'elope', 'grump', 'wafts', 'soots', 'blimp', 'hefts', 'mulls', 'hosed', 'pixie', 'waifs', 'ousts', 
'pucks', 'biers', 'gulch', 'suets', 'hobos', 'lints', 'brans', 'teals', 'garbs', 'pewee', 'helms', 'turfs', 'quips', 'wends', 'banes', 'napes', 'icier', 'swats', 'bagel', 'hexed', 'ogres', 'goner', 'gilds', 'pyres', 'lards', 'bides', 'paged', 'talon', 'flout', 'medic', 'veals', 'putts', 'dirks', 'dotes', 'tippy', 'blurt', 'piths', 'acing', 'barer', 'whets', 'gaits', 'wools', 'dunks', 'heros', 'swabs', 'dirts', 'jutes', 'hemps', 'surfs', 'okapi', 'chows', 'shoos', 'dusks', 'parry', 'decal', 'furls', 'cilia', 'sears', 'novae', 'murks', 'warps', 'slues', 'lamer', 'saris', 'weans', 'purrs', 'dills', 'togas', 'newts', 'meany', 'bunts', 'razes', 'goons', 'wicks', 'ruses', 'vends', 'geode', 'drake', 'judos', 'lofts', 'pulps', 'lauds', 'mucks', 'vises', 'mocha', 'oiled', 'roman', 'ethyl', 'gotta', 'fugue', 'smack', 'gourd', 'bumpy', 'radix', 'fatty', 'borax', 'cubit', 'cacti', 'gamma', 'focal', 
'avail', 'papal', 'golly', 'elite', 'versa', 'billy', 'adieu', 'annum', 'howdy', 'rhino', 'norms', 'bobby', 'axiom', 'setup', 'yolks', 'terns', 'mixer', 'genre', 'knoll', 'abode', 'junta', 'gorge', 'combo', 'alpha', 'overt', 'kinda', 'spelt', 'prick', 'nobly', 'ephod', 'audio', 'modal', 'veldt', 'warty', 'fluke', 'bonny', 'bream', 'rosin', 'bolls', 'doers', 'downs', 'beady', 'motif', 'humph', 'fella', 'mould', 'crepe', 'kerns', 'aloha', 'glyph', 'azure', 'riser', 'blest', 'locus', 'lumpy', 'beryl', 'wanna', 'brier', 'tuner', 'rowdy', 'mural', 'timer', 'canst', 'krill', 'quoth', 'lemme', 'triad', 'tenon', 'amply', 'deeps', 'padre', 'leant', 'pacer', 'octal', 'dolly', 'trans', 'sumac', 'foamy', 'lolly', 'giver', 'quipu', 'codex', 'manna', 'unwed', 'vodka', 'ferny', 'salon', 'duple', 'boron', 'revue', 'crier', 'alack', 'inter', 'dilly', 'whist', 'cults', 'spake', 'reset', 'loess', 'decor', 
'mover', 'verve', 'ethic', 'gamut', 'lingo', 'dunno', 'align', 'sissy', 'incur', 'reedy', 'avant', 'piper', 'waxer', 'calyx', 'basil', 'coons', 'seine', 'piney', 'lemma', 'trams', 'winch', 'whirr', 'saith', 'ionic', 'heady', 'harem', 'tummy', 'sally', 'shied', 'dross', 'farad', 'saver', 'tilde', 'jingo', 'bower', 'serif', 'facto', 'belle', 'inset', 'bogus', 'caved', 'forte', 'sooty', 'bongo', 'toves', 'credo', 'basal', 'yella', 'aglow', 'glean', 'gusto', 'hymen', 'ethos', 'terra', 'brash', 'scrip', 'swash', 'aleph', 'tinny', 'itchy', 'wanta', 'trice', 'jowls', 'gongs', 'garde', 'boric', 'twill', 'sower', 'henry', 'awash', 'libel', 'spurn', 'sabre', 'rebut', 'penal', 'obese', 'sonny', 'quirt', 'mebbe', 'tacit', 'greek', 'xenon', 'hullo', 'pique', 'roger', 'negro', 'hadst', 'gecko', 'beget', 'uncut', 'aloes', 'louis', 'quint', 'clunk', 'raped', 'salvo', 'diode', 'matey', 'hertz', 'xylem', 
'kiosk', 'apace', 'cawed', 'peter', 'wench', 'cohos', 'sorta', 'gamba', 'bytes', 'tango', 'nutty', 'axial', 'aleck', 'natal', 'clomp', 'gored', 'siree', 'bandy', 'gunny', 'runic', 'whizz', 'rupee', 'fated', 'wiper', 'bards', 'briny', 'staid', 'hocks', 'ochre', 'yummy', 'gents', 'soupy', 'roper', 'swath', 'cameo', 'edger', 'spate', 'gimme', 'ebbed', 'breve', 'theta', 'deems', 'dykes', 'servo', 'telly', 'tabby', 'tares', 'blocs', 'welch', 'ghoul', 'vitae', 'cumin', 'dinky', 'bronc', 'tabor', 'teeny', 'comer', 'borer', 'sired', 'privy', 'mammy', 'deary', 'gyros', 'sprit', 'conga', 'quire', 'thugs', 'furor', 'bloke', 'runes', 'bawdy', 'cadre', 'toxin', 'annul', 'egged', 'anion', 'nodes', 'picky', 'stein', 'jello', 'audit', 'echos', 'fagot', 'letup', 'eyrie', 'fount', 'caped', 'axons', 'amuck', 'banal', 'riled', 'petit', 'umber', 'fibre', 'agave', 'bated', 'bilge'];

allwords = ["cigar","rebut","sissy","humph","awake","blush","focal","evade","naval","serve","heath","dwarf","model","karma","stink","grade","quiet","bench","abate","feign","major","death","fresh","crust","stool","colon","abase","marry","react","batty","pride","floss","helix","croak","staff","paper","unfed","whelp","trawl","outdo","adobe","crazy","sower","repay","digit","crate","cluck","spike","mimic","pound","maxim","linen","unmet","flesh","booby","forth","first","stand","belly","ivory","seedy","print","yearn","drain","bribe","stout","panel","crass","flume","offal","agree","error","swirl","argue","bleed","delta","flick","totem","wooer","front","shrub","parry","biome","lapel","start","greet","goner","golem","lusty","loopy","round","audit","lying","gamma","labor","islet","civic","forge","corny","moult","basic","salad","agate","spicy","spray","essay","fjord","spend","kebab","guild","aback","motor","alone","hatch","hyper","thumb","dowry","ought","belch","dutch","pilot","tweed","comet","jaunt","enema","steed","abyss","growl","fling","dozen","boozy","erode","world","gouge","click","briar","great","altar","pulpy","blurt","coast","duchy","groin","fixer","group","rogue","badly","smart","pithy","gaudy","chill","heron","vodka","finer","surer","radio","rouge","perch","retch","wrote","clock","tilde","store","prove","bring","solve","cheat","grime","exult","usher","epoch","triad","break","rhino","viral","conic","masse","sonic","vital","trace","using","peach","champ","baton","brake","pluck","craze","gripe","weary","picky","acute","ferry","aside","tapir","troll","unify","rebus","boost","truss","siege","tiger","banal","slump","crank","gorge","query","drink","favor","abbey","tangy","panic","solar","shire","proxy","point","robot","prick","wince","crimp","knoll","sugar","whack","mount","perky","could","wrung","light","those","moist","shard","pleat","aloft","skill","elder","frame","humor","pause","ulcer","ultra","robin","cynic","aroma","caulk","shake","dodge","swill","tacit","other","thorn","trove","bloke","vivid","spill","chant","choke","rupee","nasty","mourn","ahead","brine","cloth","hoard","sweet","month","lapse","watch","today","focus","smelt","tease","cater","movie","saute","allow","renew","their","slosh","purge","chest","depot","epoxy","nymph","found","shall","stove","lowly","snout","trope","fewer","shawl","natal","comma","foray","scare","stair","black","squad","royal","chunk","mince","shame","cheek","ample","flair","foyer","cargo","oxide","plant","olive","inert","askew","heist","shown","zesty","trash","larva","forgo","story","hairy","train","homer","badge","midst","canny","shine","gecko","farce","slung","tipsy","metal","yield","delve","being","scour","glass","gamer","scrap","money","hinge","album","vouch","asset","tiara","crept","bayou","atoll","manor","creak","showy","phase","froth","depth","gloom","flood","trait","girth","piety","goose","float","donor","atone","primo","apron","blown","cacao","loser","input","gloat","awful","brink","smite","beady","rusty","retro","droll","gawky","hutch","pinto","egret","lilac","sever","field","fluff","agape","voice","stead","berth","madam","night","bland","liver","wedge","roomy","wacky","flock","angry","trite","aphid","tryst","midge","power","elope","cinch","motto","stomp","upset","bluff","cramp","quart","coyly","youth","rhyme","buggy","alien","smear","unfit","patty","cling","glean","label","hunky","khaki","poker","gruel","twice","twang","shrug","treat","waste","merit","woven","needy","clown","irony","ruder","gauze","chief","onset","prize","fungi","charm","gully","inter","whoop","taunt","leery","class","theme","lofty","tibia","booze","alpha","thyme","doubt","parer","chute","stick","trice","alike","recap","saint","glory","grate","admit","brisk","soggy","usurp","scald","scorn","leave","twine","sting","bough","marsh","sloth","dandy","vigor","howdy","enjoy","valid","ionic","equal","floor","catch","spade","stein","exist","quirk","denim","grove","spiel","mummy","fault","foggy","flout","carry","sneak","libel","waltz","aptly","piney","inept","aloud","photo","dream","stale","unite","snarl","baker","there","glyph","pooch","hippy","spell","folly","louse","gulch","vault","godly","threw","fleet","grave","inane","shock","crave","spite","valve","skimp","claim","rainy","musty","pique","daddy","quasi","arise","aging","valet","opium","avert","stuck","recut","mulch","genre","plume","rifle","count","incur","total","wrest","mocha","deter","study","lover","safer","rivet","funny","smoke","mound","undue","sedan","pagan","swine","guile","gusty","equip","tough","canoe","chaos","covet","human","udder","lunch","blast","stray","manga","melee","lefty","quick","paste","given","octet","risen","groan","leaky","grind","carve","loose","sadly","spilt","apple","slack","honey","final","sheen","eerie","minty","slick","derby","wharf","spelt","coach","erupt","singe","price","spawn","fairy","jiffy","filmy","stack","chose","sleep","ardor","nanny","niece","woozy","handy","grace","ditto","stank","cream","usual","diode","valor","angle","ninja","muddy","chase","reply","prone","spoil","heart","shade","diner","arson","onion","sleet","dowel","couch","palsy","bowel","smile","evoke","creek","lance","eagle","idiot","siren","built","embed","award","dross","annul","goody","frown","patio","laden","humid","elite","lymph","edify","might","reset","visit","gusto","purse","vapor","crock","write","sunny","loath","chaff","slide","queer","venom","stamp","sorry","still","acorn","aping","pushy","tamer","hater","mania","awoke","brawn","swift","exile","birch","lucky","freer","risky","ghost","plier","lunar","winch","snare","nurse","house","borax","nicer","lurch","exalt","about","savvy","toxin","tunic","pried","inlay","chump","lanky","cress","eater","elude","cycle","kitty","boule","moron","tenet","place","lobby","plush","vigil","index","blink","clung","qualm","croup","clink","juicy","stage","decay","nerve","flier","shaft","crook","clean","china","ridge","vowel","gnome","snuck","icing","spiny","rigor","snail","flown","rabid","prose","thank","poppy","budge","fiber","moldy","dowdy","kneel","track","caddy","quell","dumpy","paler","swore","rebar","scuba","splat","flyer","horny","mason","doing","ozone","amply","molar","ovary","beset","queue","cliff","magic","truce","sport","fritz","edict","twirl","verse","llama","eaten","range","whisk","hovel","rehab","macaw","sigma","spout","verve","sushi","dying","fetid","brain","buddy","thump","scion","candy","chord","basin","march","crowd","arbor","gayly","musky","stain","dally","bless","bravo","stung","title","ruler","kiosk","blond","ennui","layer","fluid","tatty","score","cutie","zebra","barge","matey","bluer","aider","shook","river","privy","betel","frisk","bongo","begun","azure","weave","genie","sound","glove","braid","scope","wryly","rover","assay","ocean","bloom","irate","later","woken","silky","wreck","dwelt","slate","smack","solid","amaze","hazel","wrist","jolly","globe","flint","rouse","civil","vista","relax","cover","alive","beech","jetty","bliss","vocal","often","dolly","eight","joker","since","event","ensue","shunt","diver","poser","worst","sweep","alley","creed","anime","leafy","bosom","dunce","stare","pudgy","waive","choir","stood","spoke","outgo","delay","bilge","ideal","clasp","seize","hotly","laugh","sieve","block","meant","grape","noose","hardy","shied","drawl","daisy","putty","strut","burnt","tulip","crick","idyll","vixen","furor","geeky","cough","naive","shoal","stork","bathe","aunty","check","prime","brass","outer","furry","razor","elect","evict","imply","demur","quota","haven","cavil","swear","crump","dough","gavel","wagon","salon","nudge","harem","pitch","sworn","pupil","excel","stony","cabin","unzip","queen","trout","polyp","earth","storm","until","taper","enter","child","adopt","minor","fatty","husky","brave","filet","slime","glint","tread","steal","regal","guest","every","murky","share","spore","hoist","buxom","inner","otter","dimly","level","sumac","donut","stilt","arena","sheet","scrub","fancy","slimy","pearl","silly","porch","dingo","sepia","amble","shady","bread","friar","reign","dairy","quill","cross","brood","tuber","shear","posit","blank","villa","shank","piggy","freak","which","among","fecal","shell","would","algae","large","rabbi","agony","amuse","bushy","copse","swoon","knife","pouch","ascot","plane","crown","urban","snide","relay","abide","viola","rajah","straw","dilly","crash","amass","third","trick","tutor","woody","blurb","grief","disco","where","sassy","beach","sauna","comic","clued","creep","caste","graze","snuff","frock","gonad","drunk","prong","lurid","steel","halve","buyer","vinyl","utile","smell","adage","worry","tasty","local","trade","finch","ashen","modal","gaunt","clove","enact","adorn","roast","speck","sheik","missy","grunt","snoop","party","touch","mafia","emcee","array","south","vapid","jelly","skulk","angst","tubal","lower","crest","sweat","cyber","adore","tardy","swami","notch","groom","roach","hitch","young","align","ready","frond","strap","puree","realm","venue","swarm","offer","seven","dryer","diary","dryly","drank","acrid","heady","theta","junto","pixie","quoth","bonus","shalt","penne","amend","datum","build","piano","shelf","lodge","suing","rearm","coral","ramen","worth","psalm","infer","overt","mayor","ovoid","glide","usage","poise","randy","chuck","prank","fishy","tooth","ether","drove","idler","swath","stint","while","begat","apply","slang","tarot","radar","credo","aware","canon","shift","timer","bylaw","serum","three","steak","iliac","shirk","blunt","puppy","penal","joist","bunny","shape","beget","wheel","adept","stunt","stole","topaz","chore","fluke","afoot","bloat","bully","dense","caper","sneer","boxer","jumbo","lunge","space","avail","short","slurp","loyal","flirt","pizza","conch","tempo","droop","plate","bible","plunk","afoul","savoy","steep","agile","stake","dwell","knave","beard","arose","motif","smash","broil","glare","shove","baggy","mammy","swamp","along","rugby","wager","quack","squat","snaky","debit","mange","skate","ninth","joust","tramp","spurn",
"medal","micro","rebel","flank","learn","nadir","maple","comfy","remit","gruff","ester","least","mogul","fetch","cause","oaken","aglow","meaty","gaffe","shyly","racer","prowl","thief","stern","poesy","rocky","tweet","waist","spire","grope","havoc","patsy","truly","forty","deity","uncle","swish","giver","preen","bevel","lemur","draft","slope","annoy","lingo","bleak","ditty","curly","cedar","dirge","grown","horde","drool","shuck","crypt","cumin","stock","gravy","locus","wider","breed","quite","chafe","cache","blimp","deign","fiend","logic","cheap","elide","rigid","false","renal","pence","rowdy","shoot","blaze","envoy","posse","brief","never","abort","mouse","mucky","sulky","fiery","media","trunk","yeast","clear","skunk","scalp","bitty","cider","koala","duvet","segue","creme","super","grill","after","owner","ember","reach","nobly","empty","speed","gipsy","recur","smock","dread","merge","burst","kappa","amity","shaky","hover","carol","snort","synod","faint","haunt","flour","chair","detox","shrew","tense","plied","quark","burly","novel","waxen","stoic","jerky","blitz","beefy","lyric","hussy","towel","quilt","below","bingo","wispy","brash","scone","toast","easel","saucy","value","spice","honor","route","sharp","bawdy","radii","skull","phony","issue","lager","swell","urine","gassy","trial","flora","upper","latch","wight","brick","retry","holly","decal","grass","shack","dogma","mover","defer","sober","optic","crier","vying","nomad","flute","hippo","shark","drier","obese","bugle","tawny","chalk","feast","ruddy","pedal","scarf","cruel","bleat","tidal","slush","semen","windy","dusty","sally","igloo","nerdy","jewel","shone","whale","hymen","abuse","fugue","elbow","crumb","pansy","welsh","syrup","terse","suave","gamut","swung","drake","freed","afire","shirt","grout","oddly","tithe","plaid","dummy","broom","blind","torch","enemy","again","tying","pesky","alter","gazer","noble","ethos","bride","extol","decor","hobby","beast","idiom","utter","these","sixth","alarm","erase","elegy","spunk","piper","scaly","scold","hefty","chick","sooty","canal","whiny","slash","quake","joint","swept","prude","heavy","wield","femme","lasso","maize","shale","screw","spree","smoky","whiff","scent","glade","spent","prism","stoke","riper","orbit","cocoa","guilt","humus","shush","table","smirk","wrong","noisy","alert","shiny","elate","resin","whole","hunch","pixel","polar","hotel","sword","cleat","mango","rumba","puffy","filly","billy","leash","clout","dance","ovate","facet","chili","paint","liner","curio","salty","audio","snake","fable","cloak","navel","spurt","pesto","balmy","flash","unwed","early","churn","weedy","stump","lease","witty","wimpy","spoof","saner","blend","salsa","thick","warty","manic","blare","squib","spoon","probe","crepe","knack","force","debut","order","haste","teeth","agent","widen","icily","slice","ingot","clash","juror","blood","abode","throw","unity","pivot","slept","troop","spare","sewer","parse","morph","cacti","tacky","spool","demon","moody","annex","begin","fuzzy","patch","water","lumpy","admin","omega","limit","tabby","macho","aisle","skiff","basis","plank","verge","botch","crawl","lousy","slain","cubic","raise","wrack","guide","foist","cameo","under","actor","revue","fraud","harpy","scoop","climb","refer","olden","clerk","debar","tally","ethic","cairn","tulle","ghoul","hilly","crude","apart","scale","older","plain","sperm","briny","abbot","rerun","quest","crisp","bound","befit","drawn","suite","itchy","cheer","bagel","guess","broad","axiom","chard","caput","leant","harsh","curse","proud","swing","opine","taste","lupus","gumbo","miner","green","chasm","lipid","topic","armor","brush","crane","mural","abled","habit","bossy","maker","dusky","dizzy","lithe","brook","jazzy","fifty","sense","giant","surly","legal","fatal","flunk","began","prune","small","slant","scoff","torus","ninny","covey","viper","taken","moral","vogue","owing","token","entry","booth","voter","chide","elfin","ebony","neigh","minim","melon","kneed","decoy","voila","ankle","arrow","mushy","tribe","cease","eager","birth","graph","odder","terra","weird","tried","clack","color","rough","weigh","uncut","ladle","strip","craft","minus","dicey","titan","lucid","vicar","dress","ditch","gypsy","pasta","taffy","flame","swoop","aloof","sight","broke","teary","chart","sixty","wordy","sheer","leper","nosey","bulge","savor","clamp","funky","foamy","toxic","brand","plumb","dingy","butte","drill","tripe","bicep","tenor","krill","worse","drama","hyena","think","ratio","cobra","basil","scrum","bused","phone","court","camel","proof","heard","angel","petal","pouty","throb","maybe","fetal","sprig","spine","shout","cadet","macro","dodgy","satyr","rarer","binge","trend","nutty","leapt","amiss","split","myrrh","width","sonar","tower","baron","fever","waver","spark","belie","sloop","expel","smote","baler","above","north","wafer","scant","frill","awash","snack","scowl","frail","drift","limbo","fence","motel","ounce","wreak","revel","talon","prior","knelt","cello","flake","debug","anode","crime","salve","scout","imbue","pinky","stave","vague","chock","fight","video","stone","teach","cleft","frost","prawn","booty","twist","apnea","stiff","plaza","ledge","tweak","board","grant","medic","bacon","cable","brawl","slunk","raspy","forum","drone","women","mucus","boast","toddy","coven","tumor","truer","wrath","stall","steam","axial","purer","daily","trail","niche","mealy","juice","nylon","plump","merry","flail","papal","wheat","berry","cower","erect","brute","leggy","snipe","sinew","skier","penny","jumpy","rally","umbra","scary","modem","gross","avian","greed","satin","tonic","parka","sniff","livid","stark","trump","giddy","reuse","taboo","avoid","quote","devil","liken","gloss","gayer","beret","noise","gland","dealt","sling","rumor","opera","thigh","tonga","flare","wound","white","bulky","etude","horse","circa","paddy","inbox","fizzy","grain","exert","surge","gleam","belle","salvo","crush","fruit","sappy","taker","tract","ovine","spiky","frank","reedy","filth","spasm","heave","mambo","right","clank","trust","lumen","borne","spook","sauce","amber","lathe","carat","corer","dirty","slyly","affix","alloy","taint","sheep","kinky","wooly","mauve","flung","yacht","fried","quail","brunt","grimy","curvy","cagey","rinse","deuce","state","grasp","milky","bison","graft","sandy","baste","flask","hedge","girly","swash","boney","coupe","endow","abhor","welch","blade","tight","geese","miser","mirth","cloud","cabal","leech","close","tenth","pecan","droit","grail","clone","guise","ralph","tango","biddy","smith","mower","payee","serif","drape","fifth","spank","glaze","allot","truck","kayak","virus","testy","tepee","fully","zonal","metro","curry","grand","banjo","axion","bezel","occur","chain","nasal","gooey","filer","brace","allay","pubic","raven","plead","gnash","flaky","munch","dully","eking","thing","slink","hurry","theft","shorn","pygmy","ranch","wring","lemon","shore","mamma","froze","newer","style","moose","antic","drown","vegan","chess","guppy","union","lever","lorry","image","cabby","druid","exact","truth","dopey","spear","cried","chime","crony","stunk","timid","batch","gauge","rotor","crack","curve","latte","witch","bunch","repel","anvil","soapy","meter","broth","madly","dried","scene","known","magma","roost","woman","thong","punch","pasty","downy","knead","whirl","rapid","clang","anger","drive","goofy","email","music","stuff","bleep","rider","mecca","folio","setup","verso","quash","fauna","gummy","happy","newly","fussy","relic","guava","ratty","fudge","femur","chirp","forte","alibi","whine","petty","golly","plait","fleck","felon","gourd","brown","thrum","ficus","stash","decry","wiser","junta","visor","daunt"]