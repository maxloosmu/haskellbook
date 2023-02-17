// https://www.youtube.com/watch?v=PAAG1zlVeZM
// https://github.com/hieunc229/copilot-clone

//find javascript binary search.
// Source: https://stackoverflow.com/a/29018745 .
function binarySearch(arr, el, compare_fn) {
    let m = 0;
    let n = arr.length - 1;
    while (m <= n) {
        let k = (n + m) >> 1;
        let cmp = compare_fn(el, arr[k]);
        if (cmp > 0) {
            m = k + 1;
        } else if(cmp < 0) {
            n = k - 1;
        } else {
            return k;
        }
    }
    return ~m;
}


function myFunction() {
  //find javascript change html DOM value.


  alert("Hello Javatpoint");
  document.getElementById("demo").innerHTML = "New text!";
}