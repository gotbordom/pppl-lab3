// Name: Antohny Tracy
// Description: I am testing for dynamically or statically scoped.


x = 0;
function f1(){  // Define a fucntion that has another function within it to run
  var x = 10;
  f2();
}

function f2(){  // Define the function that will be called within the other one
  y = x+10;
}

f1();
console.log(y);  // Returns 10 - Since JS is statically scoped
console.log(x);  // Returns 0

// NOTES: If this were dynamically scoped f2 would get the scope from f1
//        Since this is statically scoped, however, var x is local to f1 not f2 and therefore
//        when f2 is called it does not inhereit the local var x and thus x (in f2) = 0 due to the global



// Description: Problem 5a.) code - using it as an example for why short-curcuiting can be nice:

// Make some object with stats:
var someObj = {
  stat1: 'A word',
  stat2: 'Another word',
  stat3: 10
}

// Now say we want to check if there is a stat in the given object?
// Case where the stat exists:
console.log(someObj.stat1 || 'no stat here')
// Case where it doesn't:
console.log(someObj.stat4 || 'no stat here')
 
