//Check equality within an array
function isEqual(a, b) {
    let indicator = a[0] == b[0] && a[1] == b[1];
    return indicator;
}

//randomly sample a size of array
function randSample(arr, size) {
    let len = arr.length;
    let tmp = len;
    for (i = 0; i < (len - size); i++) {
        arr.splice(Math.floor(Math.random() * tmp), 1);
        tmp = tmp - 1;
    }
    return arr;
}

//check whether a is inside range_b
function checkInside(a, range_b) {
    let boolean = false;
    for (let i = 0; i < range_b.length; i++) {
        if (a == range_b[i]) {
            boolean = true;
            break;
        }
    }
    return boolean;
}

//create a range of number with start, end and step
var range = function (start, end, step) {
    var range = [];
    var typeofStart = typeof start;
    var typeofEnd = typeof end;

    if (step === 0) {
        throw TypeError("Step cannot be zero.");
    }

    if (typeofStart == "undefined" || typeofEnd == "undefined") {
        throw TypeError("Must pass start and end arguments.");
    } else if (typeofStart != typeofEnd) {
        throw TypeError("Start and end arguments must be of same type.");
    }

    typeof step == "undefined" && (step = 1);

    if (end < start) {
        step = -step;
    }

    if (typeofStart == "number") {

        while (step > 0 ? end >= start : end <= start) {
            range.push(start);
            start += step;
        }

    } else if (typeofStart == "string") {

        if (start.length != 1 || end.length != 1) {
            throw TypeError("Only strings with one character are supported.");
        }

        start = start.charCodeAt(0);
        end = end.charCodeAt(0);

        while (step > 0 ? end >= start : end <= start) {
            range.push(String.fromCharCode(start));
            start += step;
        }

    } else {
        throw TypeError("Only string and number types are supported");
    }

    return range;
};

//shuffle function
function shuffle(array) {
    return array.sort(() => Math.random() - 0.5);
}

//repeat function
function fillArray(value, len) {
    var arr = [];
    for (var i = 0; i < len; i++) {
        arr = arr.concat(value);
    }
    return arr;
}

//repeat each element in an array for n times
function repeat_each(arr, times) {
    let len = arr.length;
    let new_arr = [];
    for (var i = 0; i < len; i++) {
        new_arr.push(fillArray(arr[i], times));
    }
    return (new_arr);
}
//repeat each element in an array for n times
function repeat_each_single(arr, times) {
    let len = arr.length;
    let new_arr = [];
    for (var i = 0; i < len; i++) {
        new_arr = new_arr.concat(fillArray(arr[i], times));
    }
    return (new_arr);
}
//repeat the whole array for n times
function repeat_whole(arr, times) {
    // let rep_arr = clone(arr);
    let new_arr = [];
    for (var i = 0; i < times; i++) {
        new_arr.push(arr);
    }
    return (new_arr);
}

//repeat the whole array for n times
function repeat_whole_single(arr, times) {
    let new_arr = arr.slice();
    for (var i = 0; i < times - 1; i++) {
        new_arr = new_arr.concat(arr);
    }
    return (new_arr);
}

//repeat each array inside the whole array n times (rep function in R on array)
function repeat_each_array(arr, times) {
    let new_arr = [];
    for (var i = 0; i < arr.length; i++) {
        for (var j = 0; j < times; j++) {
            new_arr.push(arr[i]);
        }
    }
    return (new_arr);
}

//clone objects (hard copy)
function clone(obj) {
    if (obj === null || typeof (obj) !== 'object' || 'isActiveClone' in obj)
        return obj;

    if (obj instanceof Date)
        var temp = new obj.constructor(); //or new Date(obj);
    else
        var temp = obj.constructor();

    for (var key in obj) {
        if (Object.prototype.hasOwnProperty.call(obj, key)) {
            obj['isActiveClone'] = null;
            temp[key] = clone(obj[key]);
            delete obj['isActiveClone'];
        }
    }
    return temp;
}

//combine two arrays (2d)
function combineArray(arr_1, arr_2) {
    let len = arr_1.length;
    let tmp = [];
    let arr_3 = [];
    for (var i = 0; i < len; i++) {
        tmp = [arr_1[i].concat(arr_2[i])];
        arr_3 = arr_3.concat(tmp);
        tmp = [];
    }
    return arr_3;
}

//add another array to a given one
function addArray(arr_1, arr_2) {
    let len = arr_1.length;
    for (var i = 0; i < len; i++) {
        arr_1[i].push(arr_2[i]);
    }
    return arr_1;
}

//concatenate two arrays (1d)
function concatArr(arr_1, arr_2) {
    let len = arr_1.length;
    let tmp = [];
    let arr_3 = [];
    for (var i = 0; i < len; i++) {
        tmp = [
            arr_1[i].concat(arr_2[i])
        ];
        arr_3 = arr_3.concat(tmp);
        tmp = [];
    }
    return arr_3;
}


//subset an array (continuous range) in an array
function subset(arr, start, end, step) {
    const range_element = range(start, end, step);
    const len = end - start + 1;
    let subset_elem = [];
    for (var i = 0; i < len; i++) {
        subset_elem.push(arr[range_element[i]]);
    }
    return (subset_elem);
}

// reconstruct 2 arrays into a 2d array in which each element is concat matched elements from each array. (matched dimensions)
function reconstruct_array(arr_1, arr_2) {
    const dim_1 = arr_1.length;
    const dim_2 = arr_1[0].length;
    let new_elem;
    let new_arr = [];
    for (i = 0; i < dim_1; i++) {
        for (j = 0; j < dim_2; j++) {
            new_elem = [arr_1[i][j]].concat([arr_2[i][j]]);
            new_arr.push(new_elem);
        }
    }
    return (new_arr);
}

function sample_beta_n(alpha, beta, n) {
    let sample = [];
    for (var i = 0; i < n; i++) {
        // sample.push(Math.round(jStat.beta.sample(alpha,beta)*100)/100);
        sample.push(jStat.beta.sample(alpha, beta));
    }
    return (sample);
}

function sample_uniform_2d(range_x, range_y) {
    let mean_coord = [];
    mean_coord.push(jStat.uniform.sample(range_x[0], range_x[1]),
        jStat.uniform.sample(range_y[0], range_y[1]));
    return mean_coord;
}

function compute_stats(arr) {
    return {
        mean: jStat(arr).mean(),
        variance: jStat(arr).variance(),
        skewness: jStat.skewness(arr),
        kurtosis: jStat.kurtosis(arr)
    };
}

function center_mean(arr1, arr2) {
    let mean1 = jStat(arr1).mean();
    let mean2 = jStat(arr2).mean();
    let offset = (mean1 - mean2) / 2;
    console.log(offset);
    let centered_arr1;
    let centered_arr2;
    if (offset < 0) {
        centered_arr1 = jStat(arr1).add(offset)[0];
        centered_arr2 = jStat(arr2).subtract(offset)[0];
    } else {
        centered_arr1 = jStat(arr1).subtract(offset)[0];
        centered_arr2 = jStat(arr2).add(offset)[0];
    }
    return {
        centered_arr1: centered_arr1,
        centered_arr2: centered_arr2
    };
}

function extract_each_x_y(arr) {
    let x = [];
    let y = [];
    for (let i = 0; i < arr.length; i++) {
        x.push(arr[i].x);
        y.push(arr[i].y);
    }
    return {
        x: x,
        y: y
    };
}

function compute_mean(coordiantes) {
    return jStat.mean(coordiantes);
}

function compute_2d_mean(coordinates) {
    let mean_x;
    let mean_y;
    let x_coordinates = extract_each_x_y(coordinates).x;
    let y_coordiantes = extract_each_x_y(coordinates).y;
    mean_x = compute_mean(x_coordinates);
    mean_y = compute_mean(y_coordiantes);
    return {
        x: mean_x,
        y: mean_y
    };
}


function randomTruncSkewNormal({
    rng = Math.random,
    range = [-Infinity, Infinity],
    mean,
    stdDev,
    skew = 0
}) {
    // Box-Muller transform
    function randomNormals(rng) {
        let u1 = 0,
            u2 = 0;
        //Convert [0,1) to (0,1)
        while (u1 === 0) u1 = rng();
        while (u2 === 0) u2 = rng();
        const R = Math.sqrt(-2.0 * Math.log(u1));
        const Θ = 2.0 * Math.PI * u2; 4;
        return [R * Math.cos(Θ), R * Math.sin(Θ)];
    }

    // Skew-normal transform
    // If a variate is either below or above the desired range,
    // we recursively call the randomSkewNormal function until
    // a value within the desired range is drawn
    function randomSkewNormal(rng, mean, stdDev, skew = 0) {
        const [u0, v] = randomNormals(rng);
        if (skew === 0) {
            const value = mean + stdDev * u0;
            if (value < range[0] || value > range[1])
                return randomSkewNormal(rng, mean, stdDev, skew);
            return value;
        }
        const sig = skew / Math.sqrt(1 + skew * skew);
        const u1 = sig * u0 + Math.sqrt(1 - sig * sig) * v;
        const z = u0 >= 0 ? u1 : -u1;
        const value = mean + stdDev * z;
        if (value < range[0] || value > range[1])
            return randomSkewNormal(rng, mean, stdDev, skew);
        return value;
    }

    return randomSkewNormal(rng, mean, stdDev, skew);
}

function getCircleX(radians, radius, mean_x) {
    if (mean_x >= canvas.width / 2) {
        return mean_x - Math.cos(radians) * radius;
    } else {
        return mean_x + Math.cos(radians) * radius;
    }
}

function getCircleY(radians, radius, mean_y) {
    if (mean_y >= canvas.height / 2) {
        return mean_y - Math.sin(radians) * radius;
    } else {
        return mean_y + Math.sin(radians) * radius;
    }
}

function ratio_to_coord(ratios, width, height) {
    let new_coord = clone(ratios);
    for (var i = 0; i < ratios.length; i++) {
        new_coord[i].x = new_coord[i].x * width;
        new_coord[i].y = new_coord[i].y * height;
    }
    return (new_coord);
}

function getTimeRemaining(endtime) {
    const total = Date.parse(endtime) - Date.parse(new Date());
    const seconds = Math.floor((total / 1000) % 60);
    const minutes = Math.floor((total / 1000 / 60) % 60);
    // const hours = Math.floor((total / (1000 * 60 * 60)) % 24);
    // const days = Math.floor((total / (1000 * 60 * 60 * 24)));
    return {
        total,
        // days,
        // hours,
        minutes,
        seconds
    };
}

function degToRad(degrees) {
    return degrees * (Math.PI / 180);
};

function radToDeg(rad) {
    return rad / (Math.PI / 180);
};
function dist_to_center(coord, width_center, height_center) {
    return (Math.sqrt((coord.x - width_center) ** 2 + (coord.y - height_center) ** 2));
}
function find_angles_1(coord, width_center, height_center) {
    return (Math.atan((height_center - coord.y) / (width_center - coord.x)));
}

function find_angles_2(coord, width_center, height_center) {
    return (Math.atan((height_center - coord.y) / (coord.x - width_center)));
}